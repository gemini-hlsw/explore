// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import cats.Endo
import clue.data.syntax._
import explore.common.ObsQueries._
import explore.common.ObsQueriesGQL._
import explore.implicits._
import explore.schemas.ObservationDB.Types._
import explore.undo.UndoableView
import lucuma.core.enum.ScienceMode
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.core.model.Observation
import lucuma.core.optics.syntax.lens._
import monocle.Lens
import explore.undo.UndoSetter
import cats.effect.SyncIO
import cats.effect.IO

object ScienceRequirementsQueries {
  case class UndoView(
    obsId:        Observation.Id,
    undoCtx:      UndoSetter[SyncIO, IO, ScienceRequirementsData]
  )(implicit ctx: AppContextIO) {
    private val undoableView = UndoableView(undoCtx)

    def apply[A](
      modelGet:  ScienceRequirementsData => A,
      modelMod:  (A => A) => ScienceRequirementsData => ScienceRequirementsData,
      remoteSet: A => EditScienceRequirementsInput => EditScienceRequirementsInput
    ): View[A] =
      undoableView.apply(
        modelGet,
        modelMod,
        value =>
          UpdateScienceRequirementsMutation
            .execute(obsId, remoteSet(value)(EditScienceRequirementsInput()))
            .void
      )

    def apply[A](
      lens:      Lens[ScienceRequirementsData, A],
      remoteSet: A => EditScienceRequirementsInput => EditScienceRequirementsInput
    ): View[A] =
      apply(lens.get, lens.modify, remoteSet)
  }

  object UpdateScienceRequirements {
    def mode(n: ScienceMode): Endo[EditScienceRequirementsInput] =
      EditScienceRequirementsInput.mode.replace(n.assign)

    def angle(w: Angle): FocalPlaneAngleInput =
      (FocalPlaneAngleInput.microarcseconds := w.toMicroarcseconds.assign)
        .runS(FocalPlaneAngleInput())
        .value

    def wavelength(w: Wavelength): WavelengthModelInput =
      (WavelengthInput.micrometers := Wavelength.decimalMicrometers
        .reverseGet(w)
        .assign)
        .runS(WavelengthModelInput())
        .value

    def spectroscopyRequirements(
      op: SpectroscopyRequirementsData
    ): Endo[EditScienceRequirementsInput] = {
      val input =
        for {
          _ <- SpectroscopyScienceRequirementsInput.wavelength := op.wavelength
                 .map(wavelength)
                 .orUnassign
          _ <- SpectroscopyScienceRequirementsInput.resolution := op.resolution.orUnassign
          _ <- SpectroscopyScienceRequirementsInput.signalToNoise := op.signalToNoise.orUnassign
          _ <- SpectroscopyScienceRequirementsInput.signalToNoiseAt := op.signalToNoiseAt
                 .map(wavelength)
                 .orUnassign
          _ <- SpectroscopyScienceRequirementsInput.wavelengthRange := op.wavelengthRange
                 .map(wavelength)
                 .orUnassign
          _ <- SpectroscopyScienceRequirementsInput.focalPlane := op.focalPlane.orUnassign
          _ <- SpectroscopyScienceRequirementsInput.focalPlaneAngle := op.focalPlaneAngle
                 .map(angle)
                 .orUnassign
          _ <- SpectroscopyScienceRequirementsInput.capabilities := op.capabilities.orUnassign
        } yield ()
      EditScienceRequirementsInput.spectroscopyRequirements.replace(
        input.runS(SpectroscopyScienceRequirementsInput()).value.assign
      )
    }
  }
}
