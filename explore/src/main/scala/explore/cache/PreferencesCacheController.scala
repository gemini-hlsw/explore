// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.cache

import cats.effect.IO
import cats.effect.kernel.Resource
import cats.syntax.all.*
import clue.StreamingClient
import crystal.Pot
import explore.common.UserPreferencesQueries.GlobalUserPreferences
import explore.common.UserPreferencesQueries.GridLayouts
import explore.model.ExploreGridLayouts
import explore.model.GlobalPreferences
import explore.model.UserPreferences
import explore.model.enums.GridLayoutSection
import explore.model.enums.LineOfSightMotion
import explore.model.layout
import explore.model.layout.LayoutsMap
import explore.utils.*
import japgolly.scalajs.react.*
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.util.Enumerated
import lucuma.react.common.ReactFnProps
import queries.common.UserPreferencesQueriesGQL.TargetPreferencesUpdates
import queries.common.UserPreferencesQueriesGQL.UserGridLayoutUpdates
import queries.common.UserPreferencesQueriesGQL.UserPreferencesUpdates
import queries.schemas.UserPreferencesDB

import scala.concurrent.duration.*

case class PreferencesCacheController(
  userId:            User.Id,
  modUserPrefrences: (Pot[UserPreferences] => Pot[UserPreferences]) => IO[Unit]
)(using client: StreamingClient[IO, UserPreferencesDB])
    extends ReactFnProps[PreferencesCacheController](PreferencesCacheController.component)
    with CacheControllerComponent.Props[UserPreferences]:
  val modState                                 = modUserPrefrences
  val onLoad: IO[Unit]                         = IO.unit
  given StreamingClient[IO, UserPreferencesDB] = client

object PreferencesCacheController
    extends CacheControllerComponent[UserPreferences, PreferencesCacheController]:

  override protected val initial: PreferencesCacheController => IO[
    (UserPreferences, fs2.Stream[IO, UserPreferences => UserPreferences])
  ] = props =>
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

    // we could think of loading target preferences at the startup but the table maybe large
    // instead we read it on the UI control on demand
    (grids, userPrefs)
      .parMapN((g, p) => UserPreferences(g, p, Map.empty))
      .map(prefs => (prefs, fs2.Stream.empty))

  override protected val updateStream: PreferencesCacheController => Resource[
    cats.effect.IO,
    fs2.Stream[cats.effect.IO, UserPreferences => UserPreferences]
  ] = props =>
    import props.given

    val updateLayouts: Resource[IO, fs2.Stream[IO, UserPreferences => UserPreferences]] =
      UserGridLayoutUpdates
        .subscribe[IO](props.userId.show)
        .ignoreGraphQLErrors
        .map:
          _.throttle(5.seconds).map: data =>
            UserPreferences.gridLayouts
              .modify(GridLayouts.updateLayouts(data.lucumaGridLayoutPositions))

    val updateGlobalPreferences: Resource[IO, fs2.Stream[IO, UserPreferences => UserPreferences]] =
      UserPreferencesUpdates
        .subscribe[IO](props.userId.show)
        .ignoreGraphQLErrors
        .map:
          _.throttle(5.seconds).map: data =>
            UserPreferences.globalPreferences
              .modify(_ => data.lucumaUserPreferencesByPk.getOrElse(GlobalPreferences.Default))

    val updateTargetPreferences: Resource[IO, fs2.Stream[IO, UserPreferences => UserPreferences]] =
      TargetPreferencesUpdates
        .subscribe[IO](props.userId.show)
        .ignoreGraphQLErrors
        .map:
          _.throttle(5.seconds).map: data =>
            data.lucumaTarget.foldLeft(identity[UserPreferences]): (acc, target) =>
              Target.Id.parse(target.targetId) match
                case Some(targetId) =>
                  Enumerated[LineOfSightMotion].fromTag(target.lineOfSightMotion.tag) match
                    case Some(losMotion) =>
                      acc >>> UserPreferences.targetPreferences
                        .modify(prefs => prefs + (targetId -> losMotion))
                    case _               => acc
                case _              => acc

    List(
      updateLayouts,
      updateGlobalPreferences,
      updateTargetPreferences
    ).sequence.map(_.reduceLeft(_.merge(_)))
