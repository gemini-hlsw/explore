// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.schemas

import clue.data.syntax._
import lucuma.core.model.Magnitude
import lucuma.core.math.Wavelength
import lucuma.core.optics.syntax.lens._

import lucuma.schemas.ObservationDB.Types.MagnitudeCreateInput
import explore.schemas.ITC.Types.{ MagnitudeCreateInput => ITCMagnitudeInput }
import explore.schemas.ITC.Types.{ WavelengthModelInput => ITCWavelengthInput }

import java.math.MathContext

import UserPreferencesDB.Types.ExploreResizableWidthInsertInput

object implicits {
  type InstrumentModes      = explore.schemas.ITC.Types.InstrumentModes
  type GmosNITCInput        = explore.schemas.ITC.Types.GmosNITCInput
  type ITCSpectroscopyInput = ITC.Types.SpectroscopyModeInput
  type ITCWavelengthInput   = ITC.Types.WavelengthModelInput

  implicit class MagnitudeOps(m: Magnitude) {
    def toCreateInput: MagnitudeCreateInput =
      MagnitudeCreateInput(m.band,
                           m.value.toDoubleValue,
                           m.error.map(_.toRational.toBigDecimal(MathContext.UNLIMITED)).orIgnore,
                           m.system.assign
      )

    def toITCInput: ITCMagnitudeInput =
      ITCMagnitudeInput(m.band,
                        m.value.toDoubleValue,
                        m.error.map(_.toRational.toBigDecimal(MathContext.UNLIMITED)).orIgnore,
                        m.system.assign
      )
  }

  implicit class WavelengthOps(w: Wavelength) {
    def toITCInput: ITCWavelengthInput =
      (ITCWavelengthInput.nanometers := Wavelength.decimalNanometers
        .reverseGet(w)
        .assign)
        .runS(ITC.Types.WavelengthModelInput())
        .value
  }

  implicit def widthUpsertInput(w: WidthUpsertInput): ExploreResizableWidthInsertInput =
    ExploreResizableWidthInsertInput(
      w.section.value.assign,
      w.user.toString.assign,
      w.width.assign
    )
}
