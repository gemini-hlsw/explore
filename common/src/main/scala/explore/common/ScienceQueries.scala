// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import cats.Endo
import clue.data.syntax._
import crystal.react.View
import crystal.react.implicits._
import explore.common.ObsQueries._
import explore.implicits._
import explore.model.ScienceMode
import explore.model.ScienceModeAdvanced
import explore.model.ScienceModeBasic
import explore.undo.UndoSetter
import lucuma.core.enum
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.core.model.Observation
import lucuma.core.optics.syntax.lens._
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
            .execute(
              BulkEditScienceRequirementsInput(
                select = BulkEditSelectInput(observationIds = List(obsId).assign),
                edit = remoteSet(value)(ScienceRequirementsInput())
              )
            )
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
    def mode(n: enum.ScienceMode): Endo[ScienceRequirementsInput] =
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

  implicit class ScienceModeBasicGmosNLongSlitOps(val b: ScienceModeBasic.GmosNorthLongSlit)
      extends AnyVal {
    def toInput: GmosNorthLongSlitBasicConfigInput =
      GmosNorthLongSlitBasicConfigInput(b.grating.assign, b.filter.orUnassign, b.fpu.assign)
  }

  implicit class ScienceModeBasicGmosSLongSlitOps(val b: ScienceModeBasic.GmosSouthLongSlit)
      extends AnyVal {
    def toInput: GmosSouthLongSlitBasicConfigInput =
      GmosSouthLongSlitBasicConfigInput(b.grating.assign, b.filter.orUnassign, b.fpu.assign)
  }

  implicit class OffsetComponentOps(val o: Offset.Component[_]) extends AnyVal {
    def toInput: OffsetComponentInput =
      OffsetComponentInput(microarcseconds = o.toAngle.toMicroarcseconds.assign)
  }

  implicit class ScienceModeAdvancedGmosNLongSlitOps(val a: ScienceModeAdvanced.GmosNorthLongSlit)
      extends AnyVal {
    def toInput: GmosNorthLongSlitAdvancedConfigInput =
      GmosNorthLongSlitAdvancedConfigInput(
        a.overrideGrating.orUnassign,
        a.overrideFilter.orUnassign,
        a.overrideFpu.orUnassign,
        a.explicitXBin.orUnassign,
        a.explicitYBin.orUnassign,
        a.explicitAmpReadMode.orUnassign,
        a.explicitAmpGain.orUnassign,
        a.explicitRoi.orUnassign,
        a.explicitWavelengthDithers.assign,
        a.explicitSpatialOffsets.map(_.toInput).assign
      )
  }

  implicit class ScienceModeAdvancedGmosSLongSlitOps(val a: ScienceModeAdvanced.GmosSouthLongSlit)
      extends AnyVal {
    def toInput: GmosSouthLongSlitAdvancedConfigInput =
      GmosSouthLongSlitAdvancedConfigInput(
        a.overrideGrating.orUnassign,
        a.overrideFilter.orUnassign,
        a.overrideFpu.orUnassign,
        a.explicitXBin.orUnassign,
        a.explicitYBin.orUnassign,
        a.explicitAmpReadMode.orUnassign,
        a.explicitAmpGain.orUnassign,
        a.explicitRoi.orUnassign,
        a.explicitWavelengthDithers.assign,
        a.explicitSpatialOffsets.map(_.toInput).assign
      )
  }

  implicit class ScienceModeOps(val b: ScienceMode) extends AnyVal {
    def toInput: ScienceModeInput =
      b match {
        case ScienceMode.GmosNorthLongSlit(basic, advanced) =>
          ScienceModeInput(
            gmosNorthLongSlit = GmosNorthLongSlitInput(
              basic = basic.toInput.assign,
              advanced = advanced.toInput.assign
            ).assign
          )
        case ScienceMode.GmosSouthLongSlit(basic, advanced) =>
          ScienceModeInput(
            gmosSouthLongSlit = GmosSouthLongSlitInput(
              basic = basic.toInput.assign,
              advanced = advanced.toInput.assign
            ).assign
          )
      }
  }
}
