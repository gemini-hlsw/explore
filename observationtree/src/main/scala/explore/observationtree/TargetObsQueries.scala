// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.data.NonEmptyList
import cats.effect.IO
import clue.GraphQLOperation
import clue.macros.GraphQL
import eu.timepit.refined.types.string.NonEmptyString
import explore.AppCtx
import explore.GraphQLSchemas._
import explore.components.graphql.LiveQueryRenderMod
import explore.data.KeyedIndexedList
import explore.implicits._
import explore.model.Constants
import explore.model.ObsSummary
import explore.model.reusability._
import io.circe.Decoder
import io.circe.HCursor
import japgolly.scalajs.react.Reusability
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Asterism
import lucuma.core.model.Observation
import lucuma.core.model.Target
import lucuma.ui.reusability._
import monocle.Getter
import monocle.macros.Lenses

object TargetObsQueries {

  @Lenses
  case class TargetIdName(id: Target.Id, name: NonEmptyString)
  object TargetIdName {
    implicit val decoder: Decoder[TargetIdName] = new Decoder[TargetIdName] {
      final def apply(c: HCursor): Decoder.Result[TargetIdName] =
        for {
          id   <- c.downField("id").as[Target.Id]
          name <-
            c.downField("name")
              .as[Option[String]]
              .map(
                _.flatMap(name => NonEmptyString.from(name).toOption)
                  .getOrElse(Constants.UnnamedTarget)
              )
        } yield TargetIdName(id, name)
    }
  }

  @Lenses
  case class AsterismIdName(
    id:      Asterism.Id,
    name:    NonEmptyString,
    targets: TargetList
  )
  object AsterismIdName {
    implicit val decoder: Decoder[AsterismIdName] = new Decoder[AsterismIdName] {
      final def apply(c: HCursor): Decoder.Result[AsterismIdName] =
        for {
          id      <- c.downField("id").as[Asterism.Id]
          name    <-
            c.downField("name")
              .as[Option[String]]
              .map(
                _.flatMap(name => NonEmptyString.from(name).toOption)
                  .getOrElse(Constants.UnnamedAsterism)
              )
          targets <- c.downField("targets").downField("nodes").as[List[TargetIdName]]
        } yield AsterismIdName(id, name, KeyedIndexedList.fromList(targets, TargetIdName.id.get))
    }
  }

  type TargetList   = KeyedIndexedList[Target.Id, TargetIdName]
  type AsterismList = KeyedIndexedList[Asterism.Id, AsterismIdName]
  type ObsList      = KeyedIndexedList[Observation.Id, ObsSummary]

  @Lenses
  case class TargetsAndAsterismsWithObs(
    targets:      TargetList,
    asterisms:    AsterismList,
    observations: ObsList
  )

  @GraphQL(debug = false)
  object TargetsObsQuery extends GraphQLOperation[ObservationDB] {
    val document = s"""
      query {
        targets(programId: "p-2", first: ${Int.MaxValue}) {
          nodes {
            id
            name
          }
        }

        asterisms(programId: "p-2", first: ${Int.MaxValue}) {
          nodes {
            id
            name
            targets(first: ${Int.MaxValue}) {
              nodes {
                id
                name
              }
            }
          }
        }

        observations(programId: "p-2", first: ${Int.MaxValue}) {
          nodes {
            id
            name
            observationTarget {
              type: __typename
              ... on Target {
                target_id: id
              }
              ... on Asterism {
                asterism_id: id
              }
            }
            constraintSet {
              id
            }            
          }
        }
      }
    """

    object Data {
      object Targets {
        type Nodes = TargetIdName
      }

      object Asterisms {
        type Nodes = AsterismIdName
      }

      object Observations {
        type Nodes = ObsSummary
      }

      private def asterismName(name: Option[String]): NonEmptyString =
        name.flatMap(n => NonEmptyString.from(n).toOption).getOrElse(Constants.UnnamedAsterism)

      val asTargetsWithObs: Getter[Data, TargetsAndAsterismsWithObs] = data => {
        TargetsAndAsterismsWithObs(
          KeyedIndexedList.fromList(data.targets.nodes, TargetIdName.id.get),
          KeyedIndexedList.fromList(data.asterisms.nodes, AsterismIdName.id.get),
          KeyedIndexedList.fromList(data.observations.nodes, ObsSummary.id.get)
        )
      }
    }
  }

  @GraphQL
  object TargetEditSubscription extends GraphQLOperation[ObservationDB] {
    val document = """
      subscription {
        targetEdit(programId: "p-2") {
          id
        }
      }    
    """
  }

  @GraphQL
  object AsterismEditSubscription extends GraphQLOperation[ObservationDB] {
    val document = """
      subscription {
        asterismEdit(programId: "p-2") {
          id
        }
      }    
    """
  }

  @GraphQL
  object ObservationEditSubscription extends GraphQLOperation[ObservationDB] {
    val document = """
      subscription {
        observationEdit(programId: "p-2") {
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

  @GraphQL
  object AddAsterism extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation($asterismId: AsterismId!, $name: String!) {
        createAsterism(input:{
          asterismId: $asterismId,
          name: $name,
          programIds: ["p-2"]
        }) {
          id
        }
      }
    """
  }

  @GraphQL
  object RemoveAsterism extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation($asterismId: AsterismId!) {
        deleteAsterism(asterismId: $asterismId) {
          id
        }
      }
    """
  }

  @GraphQL
  object UndeleteAsterism extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation($asterismId: AsterismId!) {
        undeleteAsterism(asterismId: $asterismId) {
          id
        }
      }    
    """
  }

  @GraphQL
  object ShareTargetWithObs extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation($targetId: TargetId!, $obsId: ObservationId!) {
        shareTargetWithObservations(
          input: { targetId: $targetId, observationIds: [$obsId] }
        ) {
          id
        }
      }
    """
  }

  @GraphQL
  object ShareAsterismWithObs extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation($asterismId: AsterismId!, $obsId: ObservationId!) {
        shareAsterismWithObservations(
          input: { asterismId: $asterismId, observationIds: [$obsId] }
        ) {
          id
        }
      }
    """
  }

  @GraphQL
  object UnassignObs extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation($obsId: ObservationId!) {
        updateObservation(
          input: { observationId: $obsId, asterismId: null, targetId: null }
        ) {
          id
        }
      }
    """
  }

  @GraphQL
  object ShareTargetWithAsterisms extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation($targetId: TargetId!, $asterismId: AsterismId!) {
        shareTargetWithAsterisms(input: { targetId: $targetId, asterismIds: [$asterismId] }) {
          id
        }
      }    
    """
  }

  @GraphQL
  object UnshareTargetWithAsterisms extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation($targetId: TargetId!, $asterismId: AsterismId!) {
        unshareTargetWithAsterisms(input: { targetId: $targetId, asterismIds: [$asterismId] }) {
          id
        }
      }    
    """
  }

  implicit val targetIdNameReusability: Reusability[TargetIdName]                 =
    Reusability.by(x => (x.id, x.name))
  implicit val asterismIdNameReusability: Reusability[AsterismIdName]             =
    Reusability.by(x => (x.id, x.name))
  implicit val targetsWithObsReusability: Reusability[TargetsAndAsterismsWithObs] =
    Reusability.derive

  type LiveQueryRenderer =
    (
      View[TargetsAndAsterismsWithObs] => VdomNode
    ) => LiveQueryRenderMod[ObservationDB, TargetsObsQuery.Data, TargetsAndAsterismsWithObs]

  val TargetObsLiveQuery: LiveQueryRenderer =
    render =>
      AppCtx.withCtx { implicit appCtx =>
        LiveQueryRenderMod[ObservationDB, TargetsObsQuery.Data, TargetsAndAsterismsWithObs](
          TargetsObsQuery.query(),
          TargetsObsQuery.Data.asTargetsWithObs.get,
          NonEmptyList.of(
            TargetEditSubscription.subscribe[IO](),
            AsterismEditSubscription.subscribe[IO](),
            ObservationEditSubscription.subscribe[IO]()
          )
        )(render)
      }
}
