// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.target

import cats.effect.IO
import clue.GraphQLOperation
import clue.macros.GraphQL
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.refineV
import eu.timepit.refined.types.string.NonEmptyString
import explore.GraphQLSchemas.ObservationDB.Types._
import explore.GraphQLSchemas._
import explore.implicits._
import explore.model.decoders._
import explore.undo.Undoer
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.RightAscension
import lucuma.core.model.Target
import lucuma.ui.reusability._
import monocle.Lens

object TargetQueries {

  @GraphQL(debug = false)
  object TargetEditQuery extends GraphQLOperation[ObservationDB] {
    val document = """
      query($id: TargetId!) {
        target(targetId: $id) {
          id
          name
          tracking {
            ... on Sidereal {
              coordinates {
                ra {
                  microarcseconds
                }
                dec {
                  microarcseconds
                }
              }
            }
          }
        }
      }
      """

    object Data {
      object Target {
        object Tracking {
          type Coordinates = lucuma.core.math.Coordinates
        }
      }
    }
  }

  type TargetResult = TargetEditQuery.Data.Target
  val TargetResult = TargetEditQuery.Data.Target

  /**
   * Lens for right ascension of a TargetResult.Tracking
   */
  val properMotionRA: Lens[TargetResult.Tracking, RightAscension] =
    TargetResult.Tracking.coordinates ^|-> Coordinates.rightAscension

  /**
   * Lens for declination of a TargetResult.Tracking
   */
  val properMotionDec: Lens[TargetResult.Tracking, Declination] =
    TargetResult.Tracking.coordinates ^|-> Coordinates.declination

  /**
   * Lens to the RightAscension of a sidereal target
   */
  val targetRA: Lens[TargetResult, RightAscension] =
    TargetResult.tracking ^|-> properMotionRA

  /**
   * Lens to the name of a sidereal target
   */
  val unsafeTargetName: Lens[TargetResult, NonEmptyString] =
    Lens[TargetResult, NonEmptyString](x =>
      refineV[NonEmpty](x.name).getOrElse(sys.error("Attempt to set an empty name"))
    )(s => n => n.copy(name = s.value))

  /**
   * Lens to the Declination of a sidereal target
   */
  val targetDec: Lens[TargetResult, Declination] =
    TargetResult.tracking ^|-> properMotionDec

  /**
   * Lens used to change name and coordinates of a target
   */
  val targetPropsL =
    Lens[TargetResult, (String, RightAscension, Declination)](t =>
      (t.name, targetRA.get(t), targetDec.get(t))
    )(s => t => targetRA.set(s._2)(targetDec.set(s._3)(t.copy(name = s._1))))

  @GraphQL
  object TargetEditSubscription extends GraphQLOperation[ObservationDB] {
    val document = """
      subscription($id: TargetId!) {
        targetEdit(targetId: $id) {
          id
        }
      }
      """
  }

  @GraphQL
  object TargetMutation extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation($input: EditSiderealInput!) {
        updateSiderealTarget(input: $input) {
          id
          name
        }
      }
    """
  }

  case class UndoSet(
    id:           Target.Id,
    view:         View[TargetResult],
    setter:       Undoer.Setter[IO, TargetResult]
  )(implicit ctx: AppContextIO) {
    def apply[A](
      lens:      Lens[TargetResult, A],
      setFields: A => EditSiderealInput => EditSiderealInput
    )(
      value:     A
    ): IO[Unit] =
      setter.set(
        view.get,
        lens.get,
        { value: A =>
          for {
            _        <- (view.mod).compose(lens.set)(value)
            editInput = setFields(value)(EditSiderealInput(id))
            _        <- TargetMutation.execute(editInput)
          } yield ()
        }
      )(value)
  }

}
