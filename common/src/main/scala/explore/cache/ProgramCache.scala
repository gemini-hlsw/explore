// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.cache

import cats.effect.IO
import japgolly.scalajs.react.*
// import japgolly.scalajs.react.feature.Context
// import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Program
// import crystal.react.hooks.*
// import lucuma.ui.reusability.given
// import explore.model.AppContext
// import lucuma.ui.syntax.pot.*
import scala.collection.immutable.SortedMap
import lucuma.core.model.Target
import cats.effect.kernel.Resource
import queries.common.TargetQueriesGQL
import explore.DefaultErrorPolicy
import lucuma.ui.reusability.given
import clue.StreamingClient
import lucuma.schemas.ObservationDB
import cats.Order.given
import monocle.Lens
import monocle.Focus
import explore.model.TargetWithObs
import scala.collection.immutable.SortedSet
import explore.common.AsterismQueries.*
import queries.common.AsterismQueriesGQL
import queries.common.AsterismQueriesGQL.AsterismGroupObsQuery
import queries.common.ObsQueriesGQL

case class ProgramCache(programId: Program.Id)(using client: StreamingClient[IO, ObservationDB]):
  given StreamingClient[IO, ObservationDB] = client

given Reusability[ProgramCache] = Reusability.by(_.programId)

object ProgramCache extends CacheComponent[ProgramCache, AsterismGroupsWithObs]:

  override protected val initial: ProgramCache => IO[AsterismGroupsWithObs] = props =>
    import props.given

    AsterismQueriesGQL
      .AsterismGroupObsQuery[IO]
      .query(props.programId)
      .map(AsterismGroupObsQuery.Data.asAsterismGroupWithObs.get)

  override protected val updateStream: ProgramCache => Resource[
    cats.effect.IO,
    fs2.Stream[cats.effect.IO, AsterismGroupsWithObs => AsterismGroupsWithObs]
  ] = props =>
    import props.given

    // TargetQueriesGQL.ProgramTargetsDelta
    //   .subscribe[IO](props.programId)
    //   .map(
    //     _.map(data =>
    //       AsterismGroupsWithObs.targetsWithObs
    //         .modify(
    //           _.updated(
    //             data.targetEdit.value.id,
    //             TargetWithObs(data.targetEdit.value.target, SortedSet.empty)
    //           )
    //         )
    //     )
    //   )

    ObsQueriesGQL.ObservationEditSubscription
