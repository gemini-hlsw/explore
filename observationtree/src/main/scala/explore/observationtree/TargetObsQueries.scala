// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.data.NonEmptyList
import cats.effect.IO
import clue.GraphQLOperation
import clue.macros.GraphQL
import explore.AppCtx
import explore.GraphQLSchemas.ObservationDB.Types._
import explore.GraphQLSchemas._
import explore.components.graphql.LiveQueryRenderMod
import explore.data.KeyedIndexedList
import explore.implicits._
import explore.model.reusability._
import io.scalaland.chimney.dsl._
import japgolly.scalajs.react.Reusability
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Observation
import lucuma.core.model.Target
import lucuma.ui.reusability._
import monocle.Getter
import monocle.macros.Lenses

object TargetObsQueries {

  @Lenses
  case class TargetIdName(id: Target.Id, name: String)

  @Lenses
  case class ObsIdNameTarget(id: Observation.Id, name: Option[String], target: TargetIdName)

  type TargetList = KeyedIndexedList[Target.Id, TargetIdName]
  type ObsList    = KeyedIndexedList[Observation.Id, ObsIdNameTarget]

  @Lenses
  case class TargetsWithObs(targets: TargetList, obs: ObsList)

  @GraphQL(debug = false)
  object TargetsObsQuery extends GraphQLOperation[ObservationDB] {
    val document = """
      query {
        targets(programId: "p-2") {
          id
          name
          observations {
            id
            name
          }
        }
      }
    """

    object Data {
      val asTargetsWithObs: Getter[Data, TargetsWithObs] = data => {

        val targetsObservations = data.targets.map { target =>
          val targetIdName = target.transformInto[TargetIdName]
          (targetIdName,
           target.observations.map(
             _.into[ObsIdNameTarget].withFieldConst(_.target, targetIdName).transform
           )
          )
        }

        TargetsWithObs(
          KeyedIndexedList.fromList(targetsObservations.map(_._1), TargetIdName.id.get),
          KeyedIndexedList.fromList(
            targetsObservations.flatMap(_._2),
            ObsIdNameTarget.id.get
          )
        )
      }
    }
  }

  @GraphQL
  object TargetUpdatedSubscription extends GraphQLOperation[ObservationDB] {
    val document = """
      subscription {
        targetEdited(programId: "p-2") {
          id
        }
      }    
    """
  }

  @GraphQL
  object TargetCreatedSubscription extends GraphQLOperation[ObservationDB] {
    val document = """
      subscription {
        targetCreated(programId: "p-2") {
          id
        }
      }    
    """
  }

  @GraphQL
  object ObservationUpdatedSubscription extends GraphQLOperation[ObservationDB] {
    val document = """
      subscription {
        observationEdited(programId: "p-2") {
          id
        }
      }    
    """
  }

  @GraphQL
  object ObservationCreatedSubscription extends GraphQLOperation[ObservationDB] {
    val document = """
      subscription {
        observationCreated(programId: "p-2") {
          id
        }
      }    
    """
  }

  @GraphQL
  object UpdateObservationMutation extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation($input: EditObservationInput!) {
        updateObservation(input: $input) {
          id
        }
      }    
    """
  }

  @GraphQL
  object AddTarget extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation($targetId: TargetId!, $name: String!) {
        createSiderealTarget(input:{
          targetId: $targetId,
          name: $name,
          programIds: ["p-2"],
          ra: {microarcseconds: 0},
          dec: {microarcseconds: 0}
        }) {
          id
        }
      }
    """
  }

  @GraphQL
  object RemoveTarget extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation($targetId: TargetId!) {
        deleteTarget(targetId: $targetId) {
          id
        }
      }
    """
  }

  @GraphQL
  object UndeleteTarget extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation($targetId: TargetId!) {
        undeleteTarget(targetId: $targetId) {
          id
        }
      }    
    """
  }

  def updateObs(input: EditObservationInput): IO[Unit] =
    AppCtx.withCtx(implicit appCtx => UpdateObservationMutation.execute(input).void)

  def insertTarget(target: TargetIdName): IO[Unit] =
    AppCtx.flatMap(implicit ctx =>
      AddTarget
        .execute(target.id, target.name)
        .void
        .handleErrorWith { _ =>
          UndeleteTarget.execute(target.id).void
        }
    )

  def removeTarget(id: Target.Id): IO[Unit] =
    AppCtx.flatMap(implicit ctx => RemoveTarget.execute(id).void)

  implicit val targetIdNameReusability: Reusability[TargetIdName]     =
    Reusability.by(x => (x.id, x.name))
  implicit val obsIdNameReusability: Reusability[ObsIdNameTarget]     = Reusability.derive
  implicit val targetsWithObsReusability: Reusability[TargetsWithObs] = Reusability.derive

  type LiveQueryRenderer =
    (
      View[TargetsWithObs] => VdomNode
    ) => LiveQueryRenderMod[ObservationDB, TargetsObsQuery.Data, TargetsWithObs]

  val TargetObsLiveQuery: LiveQueryRenderer =
    render =>
      AppCtx.withCtx { implicit appCtx =>
        LiveQueryRenderMod[ObservationDB, TargetsObsQuery.Data, TargetsWithObs](
          TargetsObsQuery.query(),
          TargetsObsQuery.Data.asTargetsWithObs.get,
          NonEmptyList.of(
            TargetUpdatedSubscription.subscribe(),
            TargetCreatedSubscription.subscribe(),
            ObservationUpdatedSubscription.subscribe(),
            ObservationCreatedSubscription.subscribe()
          )
        )(render)
      }
}
