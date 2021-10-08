// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import cats.Endo
import cats.syntax.all._
import clue.data.syntax._
import crystal.react.implicits._
import explore.implicits._
import explore.schemas.implicits._
import lucuma.core.math.Declination
import lucuma.core.math.Epoch
import lucuma.core.math.Parallax
import lucuma.core.math.ProperMotion
import lucuma.core.math.RadialVelocity
import lucuma.core.math.RightAscension
import lucuma.core.math.units.CentimetersPerSecond
import lucuma.core.model.CatalogId
import lucuma.core.model.Magnitude
import lucuma.core.model.SiderealTarget
import lucuma.core.model.SiderealTracking
import lucuma.core.model.Target
import lucuma.schemas.ObservationDB.Types._
import monocle.Lens

import TargetQueriesGQL._
import scala.collection.immutable.SortedMap
import lucuma.core.enum.MagnitudeBand

object TargetQueries {

  case class UndoView(
    id:           Target.Id,
    undoCtx:      UndoCtx[SiderealTarget]
  )(implicit ctx: AppContextIO) {
    def apply[A](
      modelGet:  SiderealTarget => A,
      modelMod:  (A => A) => SiderealTarget => SiderealTarget,
      remoteSet: A => EditSiderealInput => EditSiderealInput
    ): View[A] =
      undoCtx
        .undoableView(modelGet, modelMod)
        .withOnMod(value =>
          SiderealTargetMutation
            .execute(
              remoteSet(value)(EditSiderealInput(SelectTargetInput(targetIds = List(id).assign)))
            )
            .void
            .runAsync
        )

    def apply[A](
      modelLens: Lens[SiderealTarget, A],
      remoteSet: A => EditSiderealInput => EditSiderealInput
    ): View[A] = apply(modelLens.get, modelLens.modify, remoteSet)
  }

  object UpdateSiderealTracking {
    def catalogId(cid: Option[CatalogId]): Endo[EditSiderealInput] =
      EditSiderealInput.catalogId.replace(
        cid.map(cid => CatalogIdInput(cid.catalog, cid.id.value)).orUnassign
      )

    def epoch(epoch: Option[Epoch]): Endo[EditSiderealInput] =
      EditSiderealInput.epoch.replace(epoch.map(Epoch.fromString.reverseGet).orUnassign)

    def ra(ra: Option[RightAscension]): Endo[EditSiderealInput] =
      EditSiderealInput.ra.replace(
        ra.map(r => RightAscensionInput(microarcseconds = r.toAngle.toMicroarcseconds.assign))
          .orUnassign
      )

    def dec(dec: Option[Declination]): Endo[EditSiderealInput] =
      EditSiderealInput.dec.replace(
        dec
          .map(d => DeclinationInput(microarcseconds = d.toAngle.toMicroarcseconds.assign))
          .orUnassign
      )

    def properMotion(
      pm: Option[ProperMotion]
    ): Endo[EditSiderealInput] =
      EditSiderealInput.properMotion.replace(
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
      EditSiderealInput.radialVelocity.replace(
        rv.map(r =>
          RadialVelocityInput(
            metersPerSecond = r.rv.withUnit[CentimetersPerSecond].value.value.assign
          )
        ).orUnassign
      )

    def parallax(p: Option[Parallax]): Endo[EditSiderealInput] =
      EditSiderealInput.parallax.replace(
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

  def replaceMagnitudes(mags: SortedMap[MagnitudeBand, Magnitude]): Endo[EditSiderealInput] =
    EditSiderealInput.magnitudes.replace(
      MagnitudeEditList(replaceList = mags.values.toList.map(_.toCreateInput).assign).assign
    )
}
