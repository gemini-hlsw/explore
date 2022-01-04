// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import cats.Endo
import cats.effect.Async
import cats.syntax.all._
import clue.TransactionalClient
import clue.data.Assign
import clue.data.syntax._
import crystal.react.View
import crystal.react.implicits._
import explore.implicits._
import explore.schemas.implicits._
import explore.undo.UndoContext
import lucuma.core.enum.MagnitudeBand
import lucuma.core.math.Declination
import lucuma.core.math.Epoch
import lucuma.core.math.Parallax
import lucuma.core.math.ProperMotion
import lucuma.core.math.RadialVelocity
import lucuma.core.math.RightAscension
import lucuma.core.model.CatalogId
import lucuma.core.model.Magnitude
import lucuma.core.model.SiderealTarget
import lucuma.core.model.SiderealTracking
import lucuma.core.model.Target
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Types._
import monocle.Lens

import scala.collection.immutable.SortedMap

import TargetQueriesGQL._

object TargetQueries {

  case class UndoView(
    id:           Target.Id,
    undoCtx:      UndoContext[SiderealTarget]
  )(implicit ctx: AppContextIO) {
    def apply[A](
      modelGet:  SiderealTarget => A,
      modelMod:  (A => A) => SiderealTarget => SiderealTarget,
      remoteSet: A => EditTargetInput => EditTargetInput
    ): View[A] =
      undoCtx
        .undoableView(modelGet, modelMod)
        .withOnMod(value =>
          UpdateTargetMutation
            .execute(
              remoteSet(value)(
                EditTargetInput(targetId = id, sidereal = EditSiderealInput().assign)
              )
            )
            .void
            .runAsync
        )

    def apply[A](
      modelLens: Lens[SiderealTarget, A],
      remoteSet: A => EditTargetInput => EditTargetInput
    ): View[A] = apply(modelLens.get, modelLens.modify, remoteSet)
  }

  def toTargetEndo(setSidereal: Endo[EditSiderealInput]): Endo[EditTargetInput] = eti =>
    eti match {
      case EditTargetInput(_, _, _, Assign(editSidereal), _) =>
        eti.copy(sidereal = setSidereal(editSidereal).assign)
      case _                                                 => eti
    }
  object UpdateSiderealTracking {

    def catalogId(cid: Option[CatalogId]): Endo[EditTargetInput] =
      toTargetEndo(EditSiderealInput.catalogId.replace(cid.map(_.toInput).orUnassign))

    def epoch(epoch: Option[Epoch]): Endo[EditTargetInput] =
      toTargetEndo(
        EditSiderealInput.epoch.replace(epoch.map(Epoch.fromString.reverseGet).orUnassign)
      )

    def ra(ra: Option[RightAscension]): Endo[EditTargetInput] =
      toTargetEndo(EditSiderealInput.ra.replace(ra.map(_.toInput).orUnassign))

    def dec(dec: Option[Declination]): Endo[EditTargetInput] =
      toTargetEndo(EditSiderealInput.dec.replace(dec.map(_.toInput).orUnassign))

    def properMotion(
      pm: Option[ProperMotion]
    ): Endo[EditTargetInput] =
      toTargetEndo(EditSiderealInput.properMotion.replace(pm.map(_.toInput).orUnassign))

    def radialVelocity(
      rv: Option[RadialVelocity]
    ): Endo[EditTargetInput] =
      toTargetEndo(EditSiderealInput.radialVelocity.replace(rv.map(_.toInput).orUnassign))

    def parallax(p: Option[Parallax]): Endo[EditTargetInput] =
      toTargetEndo(EditSiderealInput.parallax.replace(p.map(_.toInput).orUnassign))

    /**
     * Updates all the fields of sideral tracking
     */
    def apply(t: SiderealTracking): Endo[EditTargetInput] =
      catalogId(t.catalogId) >>>
        ra(t.baseCoordinates.ra.some) >>>
        dec(t.baseCoordinates.dec.some) >>>
        epoch(t.epoch.some) >>>
        properMotion(t.properMotion) >>>
        radialVelocity(t.radialVelocity) >>>
        parallax(t.parallax)
  }

  def replaceMagnitudes(mags: SortedMap[MagnitudeBand, Magnitude]): Endo[EditTargetInput] =
    toTargetEndo(
      EditSiderealInput.magnitudes.replace(
        MagnitudeEditList(replaceList = mags.values.toList.map(_.toCreateInput).assign).assign
      )
    )

  def createSiderealTarget[F[_]: Async](
    target:     SiderealTarget
  )(implicit c: TransactionalClient[F, ObservationDB]): F[Target.Id] = {
    val input = CreateTargetInput(
      programId = "p-2",
      sidereal = target.toCreateInput.assign
    )
    CreateTargetMutation.execute[F](input).map(_.createTarget.id)
  }
}
