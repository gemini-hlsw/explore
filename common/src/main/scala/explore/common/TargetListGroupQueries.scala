// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import cats.Order
import cats.effect.Async
import cats.effect.IO
import cats.implicits._
import clue.TransactionalClient
import clue.data.syntax._
import crystal.react.reuse._
import explore.AppCtx
import explore.components.graphql.LiveQueryRenderMod
import explore.implicits._
import explore.model.ObsSummaryWithConstraints
import explore.model.TargetEnv
import explore.model.TargetEnvIdObsIdSet
import explore.schemas.implicits._
import explore.utils._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.VdomNode
import lucuma.core.model.Observation
import lucuma.core.model.Target
import lucuma.core.model.TargetEnvironment
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Types._
import lucuma.ui.reusability._
import monocle.Focus
import monocle.Getter

import scala.collection.immutable.SortedMap

import TargetListGroupQueriesGQL._

object TargetListGroupQueries {
  // The default cats ordering for sorted set sorts by size first, then contents. That's not what we want.
  // This is used for sorting the TargetListGroupObsList. If we change to sort by name or something
  // else, we can remove this.
  implicit val orderSortedSet: Order[TargetEnvIdObsIdSet] =
    Order.by(_.toList.map(t => (t._2, t._1)))

  type ObservationResult = TargetListGroupObsQuery.Data.Observations.Nodes
  val ObservationResult = TargetListGroupObsQuery.Data.Observations.Nodes

  type TargetListGroupList = SortedMap[TargetEnvIdObsIdSet, TargetEnv]
  type ObsList             = SortedMap[Observation.Id, ObsSummaryWithConstraints]

  case class TargetListGroupWithObs(
    targetListGroups: TargetListGroupList,
    observations:     ObsList
  ) {
    // TODO: When the API supports getting all target environments for a program,
    // include a Map[TargetEnvironment.Id, NonEmptySet[Target.Id]] in the case class.
    // then use that map to find the target ids. Until then, this will not work
    // for unmoored targets when the whole group is not selected.
    def targetIdsFor(targetEnvObsIds: TargetEnvIdObsIdSet): Set[Target.Id] =
      targetListGroups
        .get(targetEnvObsIds)
        .fold(observations.mapFilter { obsSumm =>
          if (targetEnvObsIds.contains((obsSumm.targetEnvId, obsSumm.id.some)))
            obsSumm.scienceTargetIds.some
          else none
        }.combineAll)(_.targetIds)
  }

  object TargetListGroupWithObs {
    val targetListGroups = Focus[TargetListGroupWithObs](_.targetListGroups)
    val observations     = Focus[TargetListGroupWithObs](_.observations)
  }

  implicit val targetListGroupWithObservationsReuse: Reusability[TargetListGroupWithObs] =
    Reusability.derive

  private def obsResultToSummary(obsR: ObservationResult): ObsSummaryWithConstraints =
    ObsSummaryWithConstraints(obsR.id,
                              obsR.constraintSet,
                              obsR.status,
                              obsR.activeStatus,
                              obsR.plannedTime.execution,
                              obsR.targets.id,
                              obsR.targets.scienceTargets.map(_.id).toSet
    )

  private val queryToTargetListGroupWithObsGetter
    : Getter[TargetListGroupObsQuery.Data, TargetListGroupWithObs] = data =>
    TargetListGroupWithObs(
      data.scienceTargetListGroup.toSortedMap(_.id),
      data.observations.nodes.map(obsResultToSummary).toSortedMap(ObsSummaryWithConstraints.id.get)
    )

  implicit class TargetListGroupObsQueryDataOps(val self: TargetListGroupObsQuery.Data.type)
      extends AnyVal {
    def asTargetListGroupWithObs = queryToTargetListGroupWithObsGetter
  }

  val TargetListGroupLiveQuery =
    ScalaFnComponent[View[TargetListGroupWithObs] ==> VdomNode](render =>
      AppCtx.using { implicit appCtx =>
        LiveQueryRenderMod[ObservationDB, TargetListGroupObsQuery.Data, TargetListGroupWithObs](
          TargetListGroupObsQuery.query().reuseAlways,
          (TargetListGroupObsQuery.Data.asTargetListGroupWithObs.get _).reuseAlways,
          List(TargetEnvQueriesGQL.ProgramTargetEnvEditSubscription.subscribe[IO]()).reuseAlways
        )(potRender(render))
      }
    )

  def replaceScienceTargetList[F[_]: Async](
    targetEnvIds: List[TargetEnvironment.Id],
    targets:      List[Target]
  )(implicit c:   TransactionalClient[F, ObservationDB]) = {
    val input = BulkReplaceTargetListInput(
      select = SelectTargetEnvironmentInput(targetEnvironments = targetEnvIds.assign),
      replace = targets.map(_.toCreateInput)
    )
    ReplaceScienceTargetListMutation.execute[F](input).void
  }
}
