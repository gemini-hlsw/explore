// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.cache

import cats.effect.IO
import cats.effect.kernel.Resource
import cats.syntax.all.*
import clue.StreamingClient
import explore.DefaultErrorPolicy
import explore.common.UserPreferencesQueries.GlobalUserPreferences
import explore.common.UserPreferencesQueries.GridLayouts
import explore.model.ExploreGridLayouts
import explore.model.GlobalPreferences
import explore.model.UserPreferences
import explore.model.enums.GridLayoutSection
import explore.model.layout
import explore.model.layout.LayoutsMap
import japgolly.scalajs.react.*
import lucuma.core.model.User
import lucuma.react.common.ReactFnProps
import lucuma.ui.reusability.given
import queries.common.UserPreferencesQueriesGQL.UserGridLayoutUpdates
import queries.common.UserPreferencesQueriesGQL.UserPreferencesUpdates
import queries.schemas.UserPreferencesDB

case class PreferencesCache(
  userId:            User.Id,
  setUserPrefrences: Option[UserPreferences] => IO[Unit]
)(using client: StreamingClient[IO, UserPreferencesDB])
    extends ReactFnProps[PreferencesCache](PreferencesCache.component)
    with CacheComponent.Props[UserPreferences]:
  val setState                                 = setUserPrefrences
  given StreamingClient[IO, UserPreferencesDB] = client

object PreferencesCache extends CacheComponent[UserPreferences, PreferencesCache]:
  given Reusability[PreferencesCache] = Reusability.by(_.userId)

  override protected val initial: PreferencesCache => IO[UserPreferences] = props =>
    import props.given

    val grids: IO[Map[GridLayoutSection, LayoutsMap]] =
      GridLayouts
        .queryLayouts[IO](props.userId.some)
        .map(
          _.fold(ExploreGridLayouts.DefaultLayouts)(l =>
            layout.mergeSectionLayoutsMaps(ExploreGridLayouts.DefaultLayouts, l)
          )
        )

    val userPrefs = GlobalUserPreferences.loadPreferences[IO](props.userId)

    (grids, userPrefs).parMapN(UserPreferences.apply)

  override protected val updateStream: PreferencesCache => Resource[
    cats.effect.IO,
    fs2.Stream[cats.effect.IO, UserPreferences => UserPreferences]
  ] = props =>
    import props.given

    val updateLayouts: Resource[IO, fs2.Stream[IO, UserPreferences => UserPreferences]] =
      UserGridLayoutUpdates
        .subscribe[IO](props.userId.show)
        .map(
          _.map(data =>
            UserPreferences.gridLayouts
              .modify(GridLayouts.updateLayouts(data.lucumaGridLayoutPositions))
          )
        )

    val updateGlobalPreferences: Resource[IO, fs2.Stream[IO, UserPreferences => UserPreferences]] =
      UserPreferencesUpdates
        .subscribe[IO](props.userId.show)
        .map(
          _.map(data =>
            UserPreferences.globalPreferences
              .modify(_ => data.lucumaUserPreferencesByPk.getOrElse(GlobalPreferences.Default))
          )
        )

    List(updateLayouts, updateGlobalPreferences).sequence.map(_.reduceLeft(_.merge(_)))
