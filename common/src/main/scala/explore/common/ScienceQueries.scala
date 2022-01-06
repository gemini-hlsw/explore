// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import cats.Endo
import cats.effect.IO
import clue.TransactionalClient
import clue.data.syntax._
import crystal.react.View
import crystal.react.implicits._
import explore.common.ObsQueries._
import explore.common.ObsQueriesGQL._
import explore.implicits._
import explore.undo.UndoSetter
import lucuma.core.enum.ScienceMode
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.core.model.Observation
import lucuma.core.optics.syntax.lens._
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Types._
import monocle.Lens

object ScienceQueries {
  case class UndoView(
    obsId:                   Observation.Id,
    scienceRequirementsUndo: UndoSetter[ScienceRequirementsData]
  )(implicit ctx:            AppContextIO) {
    def apply[A](
      modelGet:  ScienceRequirementsData => A,
      modelMod:  (A => A) => ScienceRequirementsData => ScienceRequirementsData,
      remoteSet: A => EditScienceRequirementsInput => EditScienceRequirementsInput
    ): View[A] =
      scienceRequirementsUndo
        .undoableView(modelGet, modelMod)
        .withOnMod(value =>
          UpdateScienceRequirementsMutation
            .execute(obsId, remoteSet(value)(EditScienceRequirementsInput()))
            .void
            .runAsync
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

    def wavelength(w: Wavelength): WavelengthInput =
      (WavelengthInput.micrometers := Wavelength.decimalMicrometers
        .reverseGet(w)
        .assign)
        .runS(WavelengthInput())
        .value

    def spectroscopyRequirements(
      op: SpectroscopyRequirementsData
    ): Endo[EditScienceRequirementsInput] = {
      val input =
        for {
          _ <- SpectroscopyScienceRequirementsInput.wavelength         := op.wavelength
                 .map(wavelength)
                 .orUnassign
          _ <- SpectroscopyScienceRequirementsInput.resolution         := op.resolution.orUnassign
          _ <- SpectroscopyScienceRequirementsInput.signalToNoise      := op.signalToNoise.orUnassign
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
      EditScienceRequirementsInput.spectroscopyRequirements.replace(
        input.runS(SpectroscopyScienceRequirementsInput()).value.assign
      )
    }
  }

  def setScienceConfiguration(obsId: Observation.Id, conf: Option[ScienceConfigurationData])(
    implicit client:                 TransactionalClient[IO, ObservationDB]
  ): IO[Unit] =
    UpdateScienceConfigurationMutation
      .execute[IO](
        obsId,
        conf
          .map(_ match {
            case ScienceConfigurationData.GmosNorthLongSlit(filter, disperser, slitWidth) =>
              CreateObservationConfigInput(gmosNorthLongSlit =
                CreateGmosNorthLongSlit(
                  filter.orUnassign,
                  disperser,
                  SlitWidthInput(microarcseconds = slitWidth.toMicroarcseconds.assign)
                ).assign
              )
            case ScienceConfigurationData.GmosSouthLongSlit(filter, disperser, slitWidth) =>
              CreateObservationConfigInput(gmosSouthLongSlit =
                CreateGmosSouthLongSlit(
                  filter.orUnassign,
                  disperser,
                  SlitWidthInput(microarcseconds = slitWidth.toMicroarcseconds.assign)
                ).assign
              )
          })
          .orUnassign
      )
      .void
}
