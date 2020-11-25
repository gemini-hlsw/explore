// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.target

import cats.Endo
import cats.effect.IO
import cats.implicits._
import clue.GraphQLOperation
import clue.macros.GraphQL
import eu.timepit.refined.auto._
import eu.timepit.refined.types.string.NonEmptyString
import explore.GraphQLSchemas.ObservationDB.Types._
import explore.GraphQLSchemas._
import explore.implicits._
import explore.model.Constants
import explore.model.decoders._
import explore.optics._
import explore.undo.Undoer
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.Epoch
import lucuma.core.math.Parallax
import lucuma.core.math.ProperMotion
import lucuma.core.math.RadialVelocity
import lucuma.core.math.RightAscension
import lucuma.core.math.units.CentimetersPerSecond
import lucuma.core.model.CatalogId
import lucuma.core.model.Magnitude
import lucuma.core.model.SiderealTracking
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
              epoch
              properMotion {
                ra {
                  microarcsecondsPerYear
                }
              	dec {
                  microarcsecondsPerYear
                }
							}
              radialVelocity {
                centimetersPerSecond
              }
              parallax {
                microarcseconds
              }
            }
          }
          magnitudes {
            value
            band
            system
          }
        }
      }
      """

    object Data {
      object Target {
        type Tracking   = lucuma.core.model.SiderealTracking
        type Magnitudes = lucuma.core.model.Magnitude
      }
    }
  }

  type TargetResult = TargetEditQuery.Data.Target
  val TargetResult = TargetEditQuery.Data.Target

  /**
   * Lens for the base coordinates of TargetResult.Tracking
   */
  val baseCoordinates: Lens[TargetResult, Coordinates] =
    TargetResult.tracking ^|-> SiderealTracking.baseCoordinates

  val baseCoordinatesRa: Lens[TargetResult, RightAscension] =
    baseCoordinates ^|-> Coordinates.rightAscension

  val baseCoordinatesDec: Lens[TargetResult, Declination] =
    baseCoordinates ^|-> Coordinates.declination

  /**
   * Lens to the name of a sidereal target
   */
  val unsafeTargetName: Lens[TargetResult, NonEmptyString] =
    TargetResult.name ^|-> Lens[String, NonEmptyString](x =>
      NonEmptyString.from(x).getOrElse(Constants.UnnamedTarget)
    )(s => _ => s.value)

  /**
   * Lens used to change name and coordinates of a target
   */
  val targetPropsL =
    Lens[TargetResult, (NonEmptyString, SiderealTracking, List[Magnitude])](t =>
      (NonEmptyString.from(t.name).getOrElse(Constants.UnnamedTarget),
       TargetResult.tracking.get(t),
       t.magnitudes
      )
    )(s =>
      TargetResult.name.set(s._1.value) >>>
        TargetResult.tracking.set(s._2) >>>
        TargetResult.magnitudes.set(s._3)
    )

  val pmRALens: Lens[TargetResult, Option[ProperMotion.RA]] =
    TargetResult.tracking ^|-> SiderealTracking.properMotion ^|-> unsafePMRALensO

  val pmDecLens: Lens[TargetResult, Option[ProperMotion.Dec]] =
    TargetResult.tracking ^|-> SiderealTracking.properMotion ^|-> unsafePMDecLensO

  val epoch: Lens[TargetResult, Epoch] =
    TargetResult.tracking ^|-> SiderealTracking.epoch

  val pxLens: Lens[TargetResult, Option[Parallax]] =
    TargetResult.tracking ^|-> SiderealTracking.parallax

  val rvLens: Lens[TargetResult, Option[RadialVelocity]] =
    TargetResult.tracking ^|-> SiderealTracking.radialVelocity

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

  object UpdateSiderealTracking {
    def catalogId(cid: Option[CatalogId]): Endo[EditSiderealInput] =
      EditSiderealInput.catalogId.set(cid.map(cid => CatalogIdInput(cid.catalog, cid.id.value)))

    def epoch(epoch: Option[Epoch]): Endo[EditSiderealInput] =
      EditSiderealInput.epoch.set(epoch.map(Epoch.fromString.reverseGet))

    def ra(ra: Option[RightAscension]): Endo[EditSiderealInput] =
      EditSiderealInput.ra.set(
        ra.map(r => RightAscensionInput(microarcseconds = r.toAngle.toMicroarcseconds.some))
      )

    def dec(dec: Option[Declination]): Endo[EditSiderealInput] =
      EditSiderealInput.dec.set(
        dec.map(d => DeclinationInput(microarcseconds = d.toAngle.toMicroarcseconds.some))
      )

    def properMotion(
      pm: Option[ProperMotion]
    ): Endo[EditSiderealInput] =
      EditSiderealInput.properMotion.set(
        pm.map(p =>
          ProperMotionInput(
            ra = ProperMotionRaInput(microarcsecondsPerYear = p.ra.μasy.value.some),
            dec = ProperMotionDecInput(microarcsecondsPerYear = p.dec.μasy.value.some)
          )
        )
      )

    def radialVelocity(
      rv: Option[RadialVelocity]
    ): Endo[EditSiderealInput] =
      EditSiderealInput.radialVelocity.set(
        rv.map(r =>
          RadialVelocityInput(
            metersPerSecond = r.rv.withUnit[CentimetersPerSecond].value.value.some
          )
        )
      )

    def parallax(p: Option[Parallax]): Endo[EditSiderealInput] =
      EditSiderealInput.parallax.set(
        p.map(p => ParallaxModelInput(microarcseconds = p.μas.value.some))
      )

    /**
     * Updates all the fields of sideral tracking
     */
    def apply(t: SiderealTracking): Endo[EditSiderealInput] =
      catalogId(t.catalogId) >>>
        ra(t.baseCoordinates.ra.some) >>>
        dec(t.baseCoordinates.dec.some) >>>
        epoch(t.epoch.some) >>>
        properMotion(t.properMotion) >>>
        radialVelocity(t.radialVelocity) >>>
        parallax(t.parallax)
  }

}
