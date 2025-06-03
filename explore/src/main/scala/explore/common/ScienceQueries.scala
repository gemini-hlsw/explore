// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import cats.Endo
import cats.effect.IO
import clue.data.Input
import clue.data.syntax.*
import crystal.react.*
import eu.timepit.refined.*
import eu.timepit.refined.numeric.Positive
import explore.model.Observation
import explore.model.ScienceRequirements
import explore.services.OdbObservationApi
import explore.syntax.ui.*
import explore.undo.UndoSetter
import explore.utils.ToastCtx
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.core.math.WavelengthDelta
import lucuma.core.optics.syntax.lens.*
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.odb.input.*
import monocle.Lens
import org.typelevel.log4cats.Logger

object ScienceQueries:

  case class ScienceRequirementsUndoView(
    obsId:                   Observation.Id,
    scienceRequirementsUndo: UndoSetter[ScienceRequirements]
  )(using odbApi: OdbObservationApi[IO])(using Logger[IO], ToastCtx[IO]):
    def apply[A](
      modelGet:  ScienceRequirements => A,
      modelMod:  (A => A) => ScienceRequirements => ScienceRequirements,
      remoteSet: A => ScienceRequirementsInput => ScienceRequirementsInput
    ): View[A] =
      scienceRequirementsUndo
        .undoableView(modelGet, modelMod)
        .withOnMod: value =>
          odbApi
            .updateObservations(
              List(obsId),
              ObservationPropertiesInput(
                scienceRequirements = remoteSet(value)(ScienceRequirementsInput()).assign
              )
            )
            .toastErrors
            .runAsync

    def apply[A](
      lens:      Lens[ScienceRequirements, A],
      remoteSet: A => ScienceRequirementsInput => ScienceRequirementsInput
    ): View[A] =
      apply(lens.get, lens.modify, remoteSet)

  object UpdateScienceRequirements:
    def scienceRequirements(
      op: ScienceRequirements
    ): Endo[ScienceRequirementsInput] =
      val input =
        for {
          _ <- ScienceRequirementsInput.exposureTimeMode := op.exposureTimeMode
                 .map(_.toInput)
                 .orUnassign
          _ <- op.scienceMode match {
                 case Some(Left(spec)) =>
                   ScienceRequirementsInput.spectroscopy := spectroscopyRequirements(spec).assign
                 case Some(Right(img)) =>
                   ScienceRequirementsInput.imaging := imagingRequirements(img).assign
                 case None             =>
                   for {
                     _ <- ScienceRequirementsInput.spectroscopy := Input.unassign
                     _ <- ScienceRequirementsInput.imaging      := Input.unassign
                   } yield ()
               }
        } yield ()
      input.runS(_).value

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

    private def spectroscopyRequirements(
      op: ScienceRequirements.Spectroscopy
    ): SpectroscopyScienceRequirementsInput =
      SpectroscopyScienceRequirementsInput(
        wavelength = op.wavelength.map(wavelength).orUnassign,
        resolution = op.resolution.orUnassign,
        wavelengthCoverage = op.wavelengthCoverage.map(wavelengthDelta).orUnassign,
        focalPlane = op.focalPlane.orUnassign,
        focalPlaneAngle = op.focalPlaneAngle.map(angle).orUnassign,
        capability = op.capability.orUnassign
      )

    private def imagingRequirements(
      op: ScienceRequirements.Imaging
    ): ImagingScienceRequirementsInput =
      ImagingScienceRequirementsInput(
        minimumFov = op.minimumFov.map(angle).orUnassign,
        narrowFilters = op.narrowFilters.map(_.value).orUnassign,
        broadFilters = op.broadFilters.map(_.value).orUnassign,
        combinedFilters = op.combinationFilters.map(_.value).orUnassign
      )
