// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import cats.Endo
import cats.effect.IO
import clue.TransactionalClient
import clue.data.syntax._
import crystal.react.View
import crystal.react.implicits._
import explore.common.ObsQueries._
import explore.implicits._
import explore.model.ScienceModeBasic
import explore.undo.UndoSetter
import lucuma.core.enum.ScienceMode
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.core.model.Observation
import lucuma.core.optics.syntax.lens._
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Types._
import monocle.Lens
import queries.common.ObsQueriesGQL._

object ScienceQueries {
  case class UndoView(
    obsId:                   Observation.Id,
    scienceRequirementsUndo: UndoSetter[ScienceRequirementsData]
  )(implicit ctx:            AppContextIO) {
    def apply[A](
      modelGet:  ScienceRequirementsData => A,
      modelMod:  (A => A) => ScienceRequirementsData => ScienceRequirementsData,
      remoteSet: A => ScienceRequirementsInput => ScienceRequirementsInput
    ): View[A] =
      scienceRequirementsUndo
        .undoableView(modelGet, modelMod)
        .withOnMod(value =>
          UpdateScienceRequirementsMutation
            .execute(obsId, remoteSet(value)(ScienceRequirementsInput()))
            .void
            .runAsync
        )

    def apply[A](
      lens:      Lens[ScienceRequirementsData, A],
      remoteSet: A => ScienceRequirementsInput => ScienceRequirementsInput
    ): View[A] =
      apply(lens.get, lens.modify, remoteSet)
  }

  object UpdateScienceRequirements {
    def mode(n: ScienceMode): Endo[ScienceRequirementsInput] =
      ScienceRequirementsInput.mode.replace(n.assign)

    def angle(w: Angle): FocalPlaneAngleInput =
      (FocalPlaneAngleInput.microarcseconds := w.toMicroarcseconds.assign)
        .runS(FocalPlaneAngleInput())
        .value

    def wavelength(w: Wavelength): WavelengthInput =
      (WavelengthInput.micrometers := Wavelength.decimalMicrometers
        .reverseGet(w)
        .assign)
        .runS(WavelengthInput())
        .value

    def spectroscopyRequirements(
      op: SpectroscopyRequirementsData
    ): Endo[ScienceRequirementsInput] = {
      val input =
        for {
          _ <- SpectroscopyScienceRequirementsInput.wavelength         := op.wavelength
                 .map(wavelength)
                 .orUnassign
          _ <-
            SpectroscopyScienceRequirementsInput.resolution := op.resolution
              .map(_.value)
              .orUnassign
          _ <- SpectroscopyScienceRequirementsInput.signalToNoise      := op.signalToNoise
                 .map(_.value)
                 .orUnassign
          _ <- SpectroscopyScienceRequirementsInput.signalToNoiseAt    := op.signalToNoiseAt
                 .map(wavelength)
                 .orUnassign
          _ <- SpectroscopyScienceRequirementsInput.wavelengthCoverage := op.wavelengthCoverage
                 .map(wavelength)
                 .orUnassign
          _ <- SpectroscopyScienceRequirementsInput.focalPlane         := op.focalPlane.orUnassign
          _ <- SpectroscopyScienceRequirementsInput.focalPlaneAngle    := op.focalPlaneAngle
                 .map(angle)
                 .orUnassign
          _ <- SpectroscopyScienceRequirementsInput.capabilities       := op.capabilities.orUnassign
        } yield ()
      ScienceRequirementsInput.spectroscopy.replace(
        input.runS(SpectroscopyScienceRequirementsInput()).value.assign
      )
    }
  }

  implicit class ScienceModeOps(val b: ScienceModeBasic) extends AnyVal {
    def toScienceInput: ScienceModeInput =
      b match {
        case ScienceModeBasic.GmosNorthLongSlit(g, f, u) =>
          ScienceModeInput(
            gmosNorthLongSlit = GmosNorthLongSlitInput(
              basic = GmosNorthLongSlitBasicConfigInput(g.assign, f.orUnassign, u.assign).assign
            ).assign
          )
        case ScienceModeBasic.GmosSouthLongSlit(g, f, u) =>
          ScienceModeInput(
            gmosSouthLongSlit = GmosSouthLongSlitInput(
              basic = GmosSouthLongSlitBasicConfigInput(g.assign, f.orUnassign, u.assign).assign
            ).assign
          )
      }
  }

  def setScienceMode(obsId: Observation.Id, conf: Option[ScienceModeBasic])(implicit
    client:                 TransactionalClient[IO, ObservationDB]
  ): IO[Unit] =
    UpdateScienceModeMutation
      .execute[IO](obsId, conf.map(_.toScienceInput).orUnassign)
      .void
}
