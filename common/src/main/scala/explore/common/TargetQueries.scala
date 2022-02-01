// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
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
import lucuma.core.math.Declination
import lucuma.core.math.Epoch
import lucuma.core.math.Parallax
import lucuma.core.math.ProperMotion
import lucuma.core.math.RadialVelocity
import lucuma.core.math.RightAscension
import lucuma.core.model.SiderealTracking
import lucuma.core.model.SourceProfile
import lucuma.core.model.Target
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Types._
import monocle.Lens

import scala.annotation.unused

import TargetQueriesGQL._

object TargetQueries {

  case class UndoView(
    id:           Target.Id,
    undoCtx:      UndoContext[Target.Sidereal]
  )(implicit ctx: AppContextIO) {
    def apply[A](
      modelGet:  Target.Sidereal => A,
      modelMod:  (A => A) => Target.Sidereal => Target.Sidereal,
      remoteSet: A => EditTargetInput => EditTargetInput
    ): View[A] =
      undoCtx
        .undoableView(modelGet, modelMod)
        .withOnMod(value =>
          UpdateTargetMutation
            .execute(
              remoteSet(value)(
                EditTargetInput(targetId = id, sidereal = SiderealInput().assign)
              )
            )
            .void
            .runAsync
        )

    def apply[A](
      modelLens: Lens[Target.Sidereal, A],
      remoteSet: A => EditTargetInput => EditTargetInput
    ): View[A] = apply(modelLens.get, modelLens.modify, remoteSet)
  }

  def toTargetEndo(setSidereal: Endo[SiderealInput]): Endo[EditTargetInput] = eti =>
    eti match {
      case EditTargetInput(_, _, _, Assign(editSidereal), _, _) =>
        eti.copy(sidereal = setSidereal(editSidereal).assign)
      case _                                                 => eti
    }
  object UpdateSiderealTracking {

    def epoch(epoch: Option[Epoch]): Endo[EditTargetInput] =
      toTargetEndo(
        SiderealInput.epoch.replace(epoch.map(Epoch.fromString.reverseGet).orUnassign)
      )

    def ra(ra: Option[RightAscension]): Endo[EditTargetInput] =
      toTargetEndo(SiderealInput.ra.replace(ra.map(_.toInput).orUnassign))

    def dec(dec: Option[Declination]): Endo[EditTargetInput] =
      toTargetEndo(SiderealInput.dec.replace(dec.map(_.toInput).orUnassign))

    def properMotion(
      pm: Option[ProperMotion]
    ): Endo[EditTargetInput] =
      toTargetEndo(SiderealInput.properMotion.replace(pm.map(_.toInput).orUnassign))

    def radialVelocity(
      rv: Option[RadialVelocity]
    ): Endo[EditTargetInput] =
      toTargetEndo(SiderealInput.radialVelocity.replace(rv.map(_.toInput).orUnassign))

    def parallax(p: Option[Parallax]): Endo[EditTargetInput] =
      toTargetEndo(SiderealInput.parallax.replace(p.map(_.toInput).orUnassign))

    /**
     * Updates all the fields of sideral tracking
     */
    def apply(t: SiderealTracking): Endo[EditTargetInput] =
      ra(t.baseCoordinates.ra.some) >>>
        dec(t.baseCoordinates.dec.some) >>>
        epoch(t.epoch.some) >>>
        properMotion(t.properMotion) >>>
        radialVelocity(t.radialVelocity) >>>
        parallax(t.parallax)
  }

  def updateSourceProfile(@unused sourceProfile: SourceProfile): Endo[EditTargetInput] =
    identity

  def createSiderealTarget[F[_]: Async](
    id:         Target.Id,
    target:     Target.Sidereal
  )(implicit c: TransactionalClient[F, ObservationDB]): F[Unit] =
    CreateTargetMutation.execute[F]("p-2", target.toCreateInput(id.some)).void
}
