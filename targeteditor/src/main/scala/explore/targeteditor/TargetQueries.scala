// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.target

import cats.effect.IO
import clue.GraphQLOperation
import clue.macros.GraphQL
import explore.GraphQLSchemas.ObservationDB.Types._
import explore.GraphQLSchemas._
import explore.implicits._
import explore.model.SiderealTarget
import explore.model.decoders._
import explore.model.reusability._
import explore.undo.Undoer
import monocle.Lens
import monocle.macros.Lenses

object TargetQueries {

  @GraphQL
  object Subscription extends GraphQLOperation[ObservationDB] {
    val document = """
      subscription ($id: uuid!) {
        targets(where: {id: {_eq: $id}}) {
          id
          name
          object_type
          ra
          dec
        }
      }
      """

    @Lenses
    case class Data(targets: List[SiderealTarget])
  }

  @GraphQL
  object Mutation extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation ($id: uuid!, $fields: targets_set_input!){
        update_targets(_set: $fields, where: {
          id: {
            _eq: $id
          }
        }) {
          affected_rows
        }
      }
    """
  }

  private def mutate(id: SiderealTarget.Id, fields: TargetsSetInput)(implicit
    ctx:                 AppContextIO
  ): IO[Unit] =
    Mutation.execute(id, fields).void

  case class UndoSet(
    id:           SiderealTarget.Id,
    view:         View[SiderealTarget],
    setter:       Undoer.Setter[IO, SiderealTarget]
  )(implicit ctx: AppContextIO) {
    def apply[A](
      lens:   Lens[SiderealTarget, A],
      fields: A => TargetsSetInput
    )(
      value:  A
    ): IO[Unit] =
      setter.set(
        view.get,
        lens.get,
        { value: A =>
          for {
            _ <- (view.mod).compose(lens.set)(value)
            _ <- mutate(id, fields(value))
          } yield ()
        }
      )(value)
  }

}
