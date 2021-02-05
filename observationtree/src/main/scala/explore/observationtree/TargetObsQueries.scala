// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all._
import clue.GraphQLOperation
import clue.macros.GraphQL
import eu.timepit.refined.types.string.NonEmptyString
import explore.AppCtx
import explore.GraphQLSchemas.ObservationDB.Types._
import explore.GraphQLSchemas._
import explore.components.graphql.LiveQueryRenderMod
import explore.data.KeyedIndexedList
import explore.implicits._
import explore.model.Constants
import explore.model.reusability._
import io.scalaland.chimney.dsl._
import japgolly.scalajs.react.Reusability
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.{ Observation, Target }
import lucuma.ui.reusability._
import monocle.Getter
import monocle.macros.Lenses
import lucuma.core.model.Asterism

object TargetObsQueries {

  @Lenses
  case class TargetIdName(id: Target.Id, name: NonEmptyString)

  @Lenses
  case class AsterismIdName(
    id:      Asterism.Id,
    targets: TargetList,
    name:    NonEmptyString = Constants.UnnamedAsterism
  )

  type ObjectId = Either[Target.Id, Asterism.Id]

  @Lenses
  case class ObsAttached(id: Observation.Id, name: Option[String], attached: ObjectId)

  type TargetList   = KeyedIndexedList[Target.Id, TargetIdName]
  type AsterismList = KeyedIndexedList[Asterism.Id, AsterismIdName]
  type ObsList      = KeyedIndexedList[Observation.Id, ObsAttached]

  @Lenses
  case class TargetsAndAsterismsWithObs(targets: TargetList, asterisms: AsterismList, obs: ObsList)

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
        asterisms(programId: "p-2") {
          id
          name
          targets {
            id
            name
          }          
          observations {
            id
            name
          }
        }        
      }
    """

    object Data {
      trait TargetProps {
        val id: Target.Id
        val name: String
      }

      trait Targets extends TargetProps

      object Asterisms {
        trait Targets extends TargetProps
      }

      private def targetName(name: String): NonEmptyString =
        NonEmptyString.from(name).getOrElse(Constants.UnnamedTarget)

      private def asterismName(name: Option[String]): NonEmptyString =
        name.flatMap(n => NonEmptyString.from(n).toOption).getOrElse(Constants.UnnamedAsterism)

      private def transformTarget(target: TargetProps): TargetIdName =
        target
          .into[TargetIdName]
          .withFieldComputed(_.name, t => targetName(t.name))
          .transform

      val asTargetsWithObs: Getter[Data, TargetsAndAsterismsWithObs] = data => {

        val targetsObservations = data.targets.map { target =>
          val targetIdName = transformTarget(target)
          (targetIdName,
           target.observations.map(
             _.into[ObsAttached].withFieldConst(_.attached, targetIdName.id.asLeft).transform
           )
          )
        }

        val asterismsObservations = data.asterisms.map { asterism =>
          val asterismIdName =
            asterism
              .into[AsterismIdName]
              .withFieldComputed(_.name, a => asterismName(a.name))
              .withFieldComputed(
                _.targets,
                a => KeyedIndexedList.fromList(a.targets.map(transformTarget), TargetIdName.id.get)
              )
              .transform
          (asterismIdName,
           asterism.observations.map(
             _.into[ObsAttached].withFieldConst(_.attached, asterismIdName.id.asRight).transform
           )
          )
        }

        TargetsAndAsterismsWithObs(
          KeyedIndexedList.fromList(targetsObservations.map(_._1), TargetIdName.id.get),
          KeyedIndexedList.fromList(asterismsObservations.map(_._1), AsterismIdName.id.get),
          KeyedIndexedList.fromList(
            // Asterisms first, so fromList's distinctBy keeps those.
            asterismsObservations.flatMap(_._2) ++ targetsObservations.flatMap(_._2),
            ObsAttached.id.get
          )
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

  // @GraphQL
  // object UnshareTargetWithObs extends GraphQLOperation[ObservationDB] {
  //   val document = """
  //     mutation($targetId: TargetId!, $obsId: ObservationId!) {
  //       unshareTargetWithObservations(
  //         input: { targetId: $targetId, observationIds: [$obsId] }
  //       ) {
  //         id
  //       }
  //     }
  //   """
  // }

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

  def moveObs(obsId: Observation.Id, to: ObjectId): IO[Unit] =
    AppCtx.withCtx { implicit appCtx =>
      // (fromTarget match {
      //   case Left(targetId)           => UnshareTargetWithObs.execute(targetId, obsId)
      //   case Right(_ /*asterismId*/ ) =>
      //     IO.unit // UnshareAsterismWithObs.execute(asterismId, obsId)
      // }) >>
      (to match {
        case Left(targetId)    => ShareTargetWithObs.execute(targetId, obsId)
        case Right(asterismId) => ShareAsterismWithObs.execute(asterismId, obsId)
      }).void
    }

  def updateObs(input: EditObservationInput): IO[Unit] =
    AppCtx.withCtx(implicit appCtx => UpdateObservationMutation.execute(input).void)

  def insertTarget(target: TargetIdName): IO[Unit] =
    AppCtx.flatMap(implicit ctx =>
      AddTarget
        .execute(target.id, target.name.value)
        .void
        .handleErrorWith { _ =>
          UndeleteTarget.execute(target.id).void
        }
    )

  def removeTarget(id: Target.Id): IO[Unit] =
    AppCtx.flatMap(implicit ctx => RemoveTarget.execute(id).void)

  def shareTargetWithAsterism(targetId: Target.Id, asterismId: Asterism.Id): IO[Unit] =
    AppCtx.flatMap(implicit ctx => ShareTargetWithAsterisms.execute(targetId, asterismId).void)

  def unshareTargetWithAsterism(targetId: Target.Id, asterismId: Asterism.Id): IO[Unit] =
    AppCtx.flatMap(implicit ctx => UnshareTargetWithAsterisms.execute(targetId, asterismId).void)

  implicit val targetIdNameReusability: Reusability[TargetIdName]                 =
    Reusability.by(x => (x.id, x.name))
  implicit val asterismIdNameReusability: Reusability[AsterismIdName]             =
    Reusability.by(x => (x.id, x.name))
  implicit val obsIdNameReusability: Reusability[ObsAttached]                     = Reusability.derive
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
