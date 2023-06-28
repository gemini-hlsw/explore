// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import cats.Endo
import cats.effect.IO
import clue.FetchClient
import clue.data.syntax.*
import crystal.react.*
import eu.timepit.refined.*
import eu.timepit.refined.numeric.Positive
import explore.DefaultErrorPolicy
import explore.model.ScienceRequirements
import explore.syntax.ui.*
import explore.undo.UndoSetter
import explore.utils.ToastCtx
import lucuma.core.enums
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.core.math.WavelengthDelta
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.optics.syntax.lens.*
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.odb.input.*
import monocle.Lens
import org.typelevel.log4cats.Logger
import queries.common.ObsQueriesGQL.*
import queries.schemas.odb.ObsQueries.*

object ScienceQueries:

  case class ScienceRequirementsUndoView(
    programId:               Program.Id,
    obsId:                   Observation.Id,
    scienceRequirementsUndo: UndoSetter[ScienceRequirements]
  )(using FetchClient[IO, ObservationDB], Logger[IO], ToastCtx[IO]):
    def apply[A](
      modelGet:  ScienceRequirements => A,
      modelMod:  (A => A) => ScienceRequirements => ScienceRequirements,
      remoteSet: A => ScienceRequirementsInput => ScienceRequirementsInput
    ): View[A] =
      scienceRequirementsUndo
        .undoableView(modelGet, modelMod)
        .withOnMod(value =>
          UpdateObservationMutation[IO]
            .execute(
              UpdateObservationsInput(
                programId = programId,
                WHERE = obsId.toWhereObservation.assign,
                SET = ObservationPropertiesInput(scienceRequirements =
                  remoteSet(value)(ScienceRequirementsInput()).assign
                )
              )
            )
            .void
            .toastErrors
            .runAsync
        )

    def apply[A](
      lens:      Lens[ScienceRequirements, A],
      remoteSet: A => ScienceRequirementsInput => ScienceRequirementsInput
    ): View[A] =
      apply(lens.get, lens.modify, remoteSet)

  object UpdateScienceRequirements:
    def mode(n: enums.ScienceMode): Endo[ScienceRequirementsInput] =
      ScienceRequirementsInput.mode.replace(n.assign)

    def angle(w: Angle): AngleInput =
      (AngleInput.microarcseconds := w.toMicroarcseconds.assign)
        .runS(AngleInput())
        .value

    def wavelength(w: Wavelength): WavelengthInput =
      (WavelengthInput.micrometers :=
        // This will always work because Wavelength.toPicometers is refined Positive
        refineV[Positive](Wavelength.decimalMicrometers.reverseGet(w)).toOption.orIgnore)
        .runS(WavelengthInput())
        .value

    def wavelengthDelta(wc: WavelengthDelta): WavelengthInput =
      wavelength(Wavelength(wc.pm))

    def spectroscopyRequirements(
      op: ScienceRequirements.Spectroscopy
    ): Endo[ScienceRequirementsInput] = {
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
                 .map(wavelengthDelta)
                 .orUnassign
          _ <- SpectroscopyScienceRequirementsInput.focalPlane         := op.focalPlane.orUnassign
          _ <- SpectroscopyScienceRequirementsInput.focalPlaneAngle    := op.focalPlaneAngle
                 .map(angle)
                 .orUnassign
          _ <- SpectroscopyScienceRequirementsInput.capability         := op.capability.orUnassign
        } yield ()
      ScienceRequirementsInput.spectroscopy.replace(
        input.runS(SpectroscopyScienceRequirementsInput()).value.assign
      )
    }
