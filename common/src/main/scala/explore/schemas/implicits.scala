// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.schemas

import clue.data.Input
import clue.data.syntax._
import explore.common.ITCQueriesGQL
import explore.modes.GmosNorthSpectroscopyRow
import lucuma.core.math.Wavelength
import lucuma.core.model.Magnitude
import lucuma.core.optics.syntax.lens._
import lucuma.schemas.ObservationDB.Types.MagnitudeCreateInput

import java.math.MathContext

import UserPreferencesDB.Types.ExploreResizableWidthInsertInput

object implicits {

  implicit class MagnitudeOps(m: Magnitude) {
    def toCreateInput: MagnitudeCreateInput =
      MagnitudeCreateInput(m.band,
                           m.value.toDoubleValue,
                           m.error.map(_.toRational.toBigDecimal(MathContext.UNLIMITED)).orIgnore,
                           m.system.assign
      )
  }

  implicit def widthUpsertInput(w: WidthUpsertInput): ExploreResizableWidthInsertInput =
    ExploreResizableWidthInsertInput(
      w.section.value.assign,
      w.user.toString.assign,
      w.width.assign
    )
}

object itcschema {
  object implicits {

    import explore.schemas.ITC.Types.{ MagnitudeCreateInput => ITCMagnitudeInput }

    type InstrumentModes = ITC.Types.InstrumentModes
    val InstrumentModes = ITC.Types.InstrumentModes
    type GmosNITCInput = ITC.Types.GmosNITCInput
    val GmosNITCInput = ITC.Types.GmosNITCInput
    type ITCWavelengthInput = ITC.Types.WavelengthModelInput
    val ITCWavelengthInput = ITC.Types.WavelengthModelInput
    type ITCSpectroscopyInput = ITC.Types.SpectroscopyModeInput
    val ITCSpectroscopyInput = ITC.Types.SpectroscopyModeInput
    type ItcError = ITCQueriesGQL.SpectroscopyITCQuery.Data.Spectroscopy.Results.Itc.ItcError
    val ItcError = ITCQueriesGQL.SpectroscopyITCQuery.Data.Spectroscopy.Results.Itc.ItcError
    type ItcSuccess = ITCQueriesGQL.SpectroscopyITCQuery.Data.Spectroscopy.Results.Itc.ItcSuccess
    val ItcSuccess = ITCQueriesGQL.SpectroscopyITCQuery.Data.Spectroscopy.Results.Itc.ItcSuccess

    implicit class WavelengthOps(val w: Wavelength) extends AnyVal {
      def toITCInput: ITCWavelengthInput =
        (ITCWavelengthInput.nanometers := Wavelength.decimalNanometers
          .reverseGet(w)
          .assign)
          .runS(ITC.Types.WavelengthModelInput())
          .value
    }

    implicit class MagnitudeOps(val m: Magnitude) extends AnyVal {
      def toITCInput: ITCMagnitudeInput =
        ITCMagnitudeInput(m.band,
                          m.value.toDoubleValue,
                          m.error.map(_.toRational.toBigDecimal(MathContext.UNLIMITED)).orIgnore,
                          m.system.assign
        )
    }

    implicit class GmosNorthSpectropyRowOps(val r: GmosNorthSpectroscopyRow) extends AnyVal {
      def toGmosNITCInput: Input[GmosNITCInput] =
        GmosNITCInput(r.disperser, r.fpu, filter = r.filter.orIgnore).assign
    }
  }
}
