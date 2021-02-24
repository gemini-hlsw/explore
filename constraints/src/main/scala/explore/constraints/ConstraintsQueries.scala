// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.constraints

import cats.effect.ContextShift
import cats.effect.IO
import clue.GraphQLOperation
import clue.data.syntax._
import clue.macros.GraphQL
import crystal.ViewF
import explore.GraphQLSchemas.ExploreDB.Types._
import explore.GraphQLSchemas._
import explore.components.graphql.SubscriptionRenderMod
import explore.implicits._
import explore.model.Constraints
import explore.model.decoders._
import explore.model.enum.CloudCover
import explore.model.enum.ImageQuality
import explore.model.enum.SkyBackground
import explore.model.enum.WaterVapor
import explore.model.reusability._
import explore.undo.Undoer
import japgolly.scalajs.react.vdom.html_<^._
import monocle.Lens
import monocle.macros.Lenses

object ConstraintsQueries {
  @GraphQL
  object Subscription extends GraphQLOperation[ExploreDB] {
    val document = """
      subscription ($id: uuid!) {
        constraints(where: {id: {_eq: $id}}) {
          id
          name
          cloud_cover
          image_quality
          sky_background
          water_vapor
        }
      }
      """

    @Lenses
    case class Data(constraints: List[Constraints])
  }

  @GraphQL
  object Mutation extends GraphQLOperation[ExploreDB] {
    val document = """
      mutation ($id: uuid!, $fields: constraints_set_input!){
        update_constraints(_set: $fields, where: {id: {_eq: $id}}) {
          affected_rows
        }
      }
    """
  }

  case class UndoViewZoom(
    id:     Constraints.Id,
    view:   View[Constraints],
    setter: Undoer.Setter[IO, Constraints]
  ) {
    def apply[A](
      lens:        Lens[Constraints, A],
      fields:      A => ConstraintsSetInput
    )(implicit cs: ContextShift[IO]): View[A] =
      ViewF[IO, A](
        lens.get(view.get),
        setter.mod(
          view.get,
          lens.get,
          { value: A =>
            for {
              _ <- view.mod.compose(lens.set)(value)
              _ <- mutate(id, fields(value))
            } yield ()
          }
        )
      )
  }

  def iqFields(iq: ImageQuality): ConstraintsSetInput =
    ConstraintsSetInput(image_quality = iq.assign)

  def ccFields(cc: CloudCover): ConstraintsSetInput =
    ConstraintsSetInput(cloud_cover = cc.assign)

  def wvFields(wv: WaterVapor): ConstraintsSetInput =
    ConstraintsSetInput(water_vapor = wv.assign)

  def sbFields(sb: SkyBackground): ConstraintsSetInput =
    ConstraintsSetInput(sky_background = sb.assign)

  private def mutate(id: Constraints.Id, fields: ConstraintsSetInput): IO[Unit] = ???
  // AppCtx.flatMap { implicit ctx =>
  //   Mutation.execute(id, fields).void
  // }

  def ConstraintsSubscription(
    id:     Constraints.Id
  )(render: View[Constraints] => VdomNode): SubscriptionRenderMod[Subscription.Data, Constraints] =
    ???
  // AppCtx.withCtx { implicit appCtx =>
  //   SubscriptionRenderMod[Subscription.Data, Constraints](
  //     Subscription.subscribe(id),
  //     _.map(
  //       Subscription.Data.constraints.composeOptional(headOption).getOption _
  //     ).unNone
  //   )(render)
  // }
}
