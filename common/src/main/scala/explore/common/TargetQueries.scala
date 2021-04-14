// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import cats.Endo
import cats.effect.IO
import cats.syntax.all._
import clue.data.syntax._
import eu.timepit.refined.auto._
import explore.implicits._
import explore.optics._
import explore.schemas.ObservationDB.Types._
import explore.schemas.implicits._
import explore.undo.UndoableView
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
import monocle.Lens

import TargetQueriesGQL._

object TargetQueries {

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
   * Lens used to change name and coordinates of a target
   */
  val targetPropsL = disjointZip(TargetResult.name, TargetResult.tracking, TargetResult.magnitudes)

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

  case class UndoView(
    id:           Target.Id,
    view:         View[TargetResult],
    setter:       Undoer.Setter[IO, TargetResult]
  )(implicit ctx: AppContextIO) {
    private val undoableView = UndoableView(view, setter)

    def apply[A](
      modelGet:  TargetResult => A,
      modelMod:  (A => A) => TargetResult => TargetResult,
      remoteSet: A => EditSiderealInput => EditSiderealInput
    ): View[A] =
      undoableView.apply(
        modelGet,
        modelMod,
        value => TargetMutation.execute(remoteSet(value)(EditSiderealInput(id))).void
      )

    def apply[A](
      modelLens: Lens[TargetResult, A],
      remoteSet: A => EditSiderealInput => EditSiderealInput
    ): View[A] = apply(modelLens.get, modelLens.modify, remoteSet)
  }

  object UpdateSiderealTracking {
    def catalogId(cid: Option[CatalogId]): Endo[EditSiderealInput] =
      EditSiderealInput.catalogId.set(
        cid.map(cid => CatalogIdInput(cid.catalog, cid.id.value)).orUnassign
      )

    def epoch(epoch: Option[Epoch]): Endo[EditSiderealInput] =
      EditSiderealInput.epoch.set(epoch.map(Epoch.fromString.reverseGet).orUnassign)

    def ra(ra: Option[RightAscension]): Endo[EditSiderealInput] =
      EditSiderealInput.ra.set(
        ra.map(r => RightAscensionInput(microarcseconds = r.toAngle.toMicroarcseconds.assign))
          .orUnassign
      )

    def dec(dec: Option[Declination]): Endo[EditSiderealInput] =
      EditSiderealInput.dec.set(
        dec
          .map(d => DeclinationInput(microarcseconds = d.toAngle.toMicroarcseconds.assign))
          .orUnassign
      )

    def properMotion(
      pm: Option[ProperMotion]
    ): Endo[EditSiderealInput] =
      EditSiderealInput.properMotion.set(
        pm.map(p =>
          ProperMotionInput(
            ra = ProperMotionComponentInput(microarcsecondsPerYear = p.ra.μasy.value.assign),
            dec = ProperMotionComponentInput(microarcsecondsPerYear = p.dec.μasy.value.assign)
          )
        ).orUnassign
      )

    def radialVelocity(
      rv: Option[RadialVelocity]
    ): Endo[EditSiderealInput] =
      EditSiderealInput.radialVelocity.set(
        rv.map(r =>
          RadialVelocityInput(
            metersPerSecond = r.rv.withUnit[CentimetersPerSecond].value.value.assign
          )
        ).orUnassign
      )

    def parallax(p: Option[Parallax]): Endo[EditSiderealInput] =
      EditSiderealInput.parallax.set(
        p.map(p => ParallaxModelInput(microarcseconds = p.μas.value.value.assign)).orUnassign
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

  def updateMagnitudes(mags: List[Magnitude]): Endo[EditSiderealInput] =
    EditSiderealInput.magnitudes.set(mags.map(_.toInput).assign)
}
