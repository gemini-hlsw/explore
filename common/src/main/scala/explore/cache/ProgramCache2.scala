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

// case class ModelCaches2 protected[cache] (target: SortedMap[Target.Id, TargetWithObs])
// TargetWithObs(t, SortedSet.empty)

// object ModelCaches2:
//   val target: Lens[ModelCaches2, SortedMap[Target.Id, TargetWithObs]] =
//     Focus[ModelCaches2](_.target)

case class ProgramCache2(programId: Program.Id)(using client: StreamingClient[IO, ObservationDB]):
  given StreamingClient[IO, ObservationDB] = client

given Reusability[ProgramCache2] = Reusability.by(_.programId)

object ProgramCache2 extends CacheComponent[ProgramCache2, AsterismGroupsWithObs]:

  override protected val initial: ProgramCache2 => IO[AsterismGroupsWithObs] = props =>
    import props.given

    AsterismQueriesGQL
      .AsterismGroupObsQuery[IO]
      .query(props.programId)
      .map(AsterismGroupObsQuery.Data.asAsterismGroupWithObs.get)

    // TargetQueriesGQL
    //   .AllProgramTargets[IO]
    //   .query(props.programId)
    //   .map(data =>
    //     ModelCaches2(
    //       SortedMap.from(
    //         data.targets.matches.map(targetWithId =>
    //           targetWithId.id -> TargetWithObs(targetWithId.target, SortedSet.empty)
    //         )
    //       )
    //     )
    //   )

  override protected val updateStream: ProgramCache2 => Resource[
    cats.effect.IO,
    fs2.Stream[cats.effect.IO, AsterismGroupsWithObs => AsterismGroupsWithObs]
  ] = props =>
    import props.given

    TargetQueriesGQL.ProgramTargetsDelta
      .subscribe[IO](props.programId)
      .map(
        _.map(data =>
          AsterismGroupsWithObs.targetsWithObs
            .modify(
              _.updated(
                data.targetEdit.value.id,
                TargetWithObs(data.targetEdit.value.target, SortedSet.empty)
              )
            )
        )
      )

// val ctx: Context[ModelCaches[IO]] = React.createContext(null) // No default value

// val Provider =
//   ScalaFnComponent
//     .withHooks[Program.Id]
//     .withPropsChildren
//     .useContext(AppContext.ctx)
//     // FIXME This will be useResource later on
//     .useEffectResultWithDepsBy((programId, _, _) => programId)((_, _, appCtx) =>
//       programId =>
//         import appCtx.given

//         ModelCaches.forProgram(programId)
//     )
//     .render((_, children, _, caches) => caches.renderPot(ctx.provide(_)(children)))
