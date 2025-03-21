// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import cats.syntax.all.*
import eu.timepit.refined.api.Refined
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.numeric.NonNegative
import eu.timepit.refined.types.numeric.NonNegBigDecimal
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.string.NonEmptyString
import explore.model.HourRange
import explore.model.display.given
import lucuma.core.data.EmailAddress
import lucuma.core.data.EmailPred
import lucuma.core.math.Angle
import lucuma.core.math.Axis
import lucuma.core.math.BrightnessValue
import lucuma.core.math.BrightnessValueRefinement
import lucuma.core.math.Offset
import lucuma.core.math.Parallax
import lucuma.core.math.ProperMotion
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.math.WavelengthDelta
import lucuma.core.math.WavelengthDither
import lucuma.core.math.validation.MathValidators
import lucuma.core.optics.Format
import lucuma.core.optics.ValidFilter
import lucuma.core.optics.ValidSplitEpi
import lucuma.core.optics.ValidWedge
import lucuma.core.syntax.display.*
import lucuma.core.syntax.string.*
import lucuma.core.validation.*
import lucuma.refined.*
import monocle.Iso
import monocle.Prism

import scala.util.Try

object ExploreModelValidators:
  type ValidFilterNEC[A] = ValidFilter[NonEmptyChain[NonEmptyString], A]

  val brightnessValidWedge: InputValidWedge[BrightnessValue] =
    InputValidWedge(
      InputValidSplitEpi
        .refinedBigDecimal[BrightnessValueRefinement]
        .withErrorMessage(_ => "Invalid brightness value".refined)
        .getValid
        .andThen(_.map(BrightnessValue(_))),
      n => Try(n.shortName).toOption.orEmpty
    )

  // TODO Move to lucuma core
  val nonNegBigDecimal: InputValidSplitEpi[NonNegBigDecimal] =
    InputValidSplitEpi.refinedBigDecimal[NonNegative]

  val nonNegInt: InputValidSplitEpi[NonNegInt] =
    InputValidSplitEpi.refinedInt[NonNegative]

  val signalToNoiseValidSplitEpi: InputValidSplitEpi[SignalToNoise] =
    nonNegBigDecimal.andThen(
      SignalToNoise.FromNonNegBigDecimalExact,
      _ => NonEmptyChain("Invalid signal to noise".refined[NonEmpty])
    )

  private def ditherInRange(
    λcentral: Wavelength,
    λmin:     Wavelength,
    λmax:     Wavelength
  ): ValidFilterNEC[WavelengthDither] =
    ValidFilter(
      λdither =>
        val adjusted = λcentral.unsafeOffset(λdither).pm.value.value
        λmin.pm.value.value < adjusted && adjusted < λmax.pm.value.value
      ,
      _ => NonEmptyChain("Dither value is outside of valid range".refined)
    )

  val ditherValidWedge: InputValidWedge[WavelengthDither] =
    InputValidWedge
      .truncatedBigDecimal(decimals = 1.refined)
      .andThen(
        WavelengthDither.decimalNanometers,
        _ => NonEmptyChain("Invalid dither value".refined[NonEmpty])
      )

  def dithersValidWedge(
    λcentral: Wavelength,
    λmin:     Wavelength,
    λmax:     Wavelength
  ): InputValidWedge[WavelengthDither] =
    ditherValidWedge.andThen(ditherInRange(λcentral, λmin, λmax).asValidWedge)

  val offsetQNELValidWedge: InputValidWedge[Option[NonEmptyList[Offset.Q]]] =
    MathValidators.truncatedAngleSignedArcSec
      .andThen(Offset.Component.angle[Axis.Q].reverse)
      .toNel(",".refined)
      .withErrorMessage(_ => "Invalid offsets".refined)
      .optional

  val hoursValidWedge: InputValidWedge[BigDecimal Refined HourRange] =
    InputValidWedge
      .truncatedBigDecimal(decimals = 2.refined)
      .andThen(
        ValidSplitEpi
          .forRefined[String, BigDecimal, HourRange](_ => "Invalid hour value")
          .toErrorsValidSplitEpiUnsafe
      )

  val wavelengthMicroValidWedge: InputValidWedge[Wavelength] =
    InputValidWedge
      .truncatedBigDecimal(4.refined)
      .andThen(
        ValidWedge
          .fromFormat(Wavelength.decimalMicrometers, _ => "Invalid Wavelength".refined[NonEmpty])
          .toErrorsValidWedge
      )

  val wavelengthNanoValidWedge: InputValidWedge[Wavelength] =
    InputValidWedge
      .truncatedBigDecimal(1.refined)
      .andThen(
        ValidWedge
          .fromFormat(Wavelength.decimalNanometers, _ => "Invalid Wavelength".refined[NonEmpty])
          .toErrorsValidWedge
      )

  private val decimalArcsecondsPrism: Prism[BigDecimal, Angle] =
    Prism[BigDecimal, Angle](Angle.signedDecimalArcseconds.reverseGet(_).some)(
      Angle.signedDecimalArcseconds.get
    )

  val decimalArcsecondsValidWedge: InputValidWedge[Angle] =
    InputValidWedge
      .truncatedBigDecimal(2.refined)
      .andThen(
        ValidWedge
          .fromPrism(decimalArcsecondsPrism, _ => "Invalid Angle".refined[NonEmpty])
          .toErrorsValidWedge
      )

  val wavelengthMicroDeltaValidWedge: InputValidWedge[WavelengthDelta] =
    wavelengthMicroValidWedge.andThen(
      Iso[Wavelength, WavelengthDelta](w => WavelengthDelta(w.pm))(wc => Wavelength(wc.pm))
    )

  val wavelengthNanoDeltaValidWedge: InputValidWedge[WavelengthDelta] =
    wavelengthNanoValidWedge.andThen(
      Iso[Wavelength, WavelengthDelta](w =>
        WavelengthDelta
          .fromIntNanometers(w.nm.value.value.toInt)
          .getOrElse(sys.error("Invalid Wavelength"))
      )(wc =>
        Wavelength
          .fromIntNanometers(wc.nm.value.value.toInt)
          .getOrElse(sys.error("Invalid Wavelength"))
      )
    )

  // Only support numbers (one or more) with an optional sign and an optional
  // decimal point with or without numbers after the decimal point
  private val bdPattern = """"-?(?:\\d+(?:\\.\\d+)?|\\.\\d+)""".r

  // Strips non-significant zeros on `reverseGet`
  private val compactDecimalString: Format[String, String] =
    Format(
      _.some,
      s =>
        if (bdPattern.matches(s)) {
          s.parseBigDecimalOption
            .map(_.bigDecimal.stripTrailingZeros.toPlainString)
            .getOrElse(s)
        } else s
    )

  val compactDecimalStringValidWedge: InputValidWedge[String] =
    InputValidWedge.fromFormat(compactDecimalString)

  val pxValidWedge: InputValidWedge[Parallax] =
    compactDecimalStringValidWedge.andThen(
      InputValidWedge
        .truncatedBigDecimal(3.refined)
        .andThen(
          Parallax.milliarcseconds.reverse,
          _ => NonEmptyChain("Invalid parallax".refined[NonEmpty])
        )
    )

  val pmRAValidWedge: InputValidWedge[ProperMotion.RA] =
    compactDecimalStringValidWedge.andThen(
      InputValidWedge
        .truncatedBigDecimal(3.refined)
        .andThen(
          ProperMotion.RA.milliarcsecondsPerYear.reverse,
          _ => NonEmptyChain("Invalid right ascencion velocity".refined[NonEmpty])
        )
    )

  val pmDecValidWedge: InputValidWedge[ProperMotion.Dec] =
    compactDecimalStringValidWedge.andThen(
      InputValidWedge
        .truncatedBigDecimal(3.refined)
        .andThen(
          ProperMotion.Dec.milliarcsecondsPerYear.reverse,
          _ => NonEmptyChain("Invalid declination velocity".refined[NonEmpty])
        )
    )

  val MailValidator: InputValidSplitEpi[EmailAddress] =
    // Scala doesn't like type aliases with refined types?
    InputValidSplitEpi.refinedString[EmailPred].asInstanceOf[InputValidSplitEpi[EmailAddress]]
