// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import cats.syntax.all.*
import eu.timepit.refined.api.Refined
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.numeric.Interval
import eu.timepit.refined.types.string.NonEmptyString
import explore.model.DitherNanoMeters
import explore.model.DitherNanoMetersRange
import explore.model.HourRange
import explore.model.display.*
import lucuma.core.math.Axis
import lucuma.core.math.Offset
import lucuma.core.math.Parallax
import lucuma.core.math.ProperMotion
import lucuma.core.math.Wavelength
import lucuma.core.math.validation.MathValidators
import lucuma.core.optics.Format
import lucuma.core.optics.ValidSplitEpi
import lucuma.core.optics.ValidWedge
import lucuma.core.validation.*
import lucuma.refined.*
import lucuma.utils.*

import scala.util.Try

object ExploreModelValidators:
  val i = ValidSplitEpi
    .forRefined[String, BigDecimal, HourRange]("Invalid hour value")

  val brightnessValidWedge: InputValidWedge[BigDecimal] =
    InputValidWedge(
      InputValidSplitEpi.bigDecimal.withErrorMessage("Invalid brightness value".refined).getValid,
      n => Try(displayBrightness.shortName(n)).toOption.orEmpty
    )

  val dithersValidSplitEpi: InputValidSplitEpi[Option[NonEmptyList[DitherNanoMeters]]] =
    InputValidSplitEpi
      .refinedBigDecimal[DitherNanoMetersRange]
      .toNel()
      .withErrorMessage("Invalid wavelength dither values".refined)
      .optional

  val offsetQNELValidWedge: InputValidWedge[Option[NonEmptyList[Offset.Q]]] =
    MathValidators.truncatedAngleSignedDegrees
      .andThen(Offset.Component.angle[Axis.Q].reverse)
      .toNel()
      .withErrorMessage("Invalid offsets".refined)
      .optional

  val hoursValidWedge: InputValidWedge[BigDecimal Refined HourRange] =
    InputValidWedge
      .truncatedBigDecimal(decimals = 2.refined)
      .andThen(
        ValidSplitEpi
          .forRefined[String, BigDecimal, HourRange]("Invalid hour value")
          .toErrorsValidSplitEpiUnsafe
      )

  val wavelengthValidWedge: InputValidWedge[Wavelength] =
    InputValidWedge
      .truncatedBigDecimal(3.refined)
      .andThen(
        ValidWedge
          .fromFormat(Wavelength.decimalMicrometers, "Invalid Wavelength".refined[NonEmpty])
          .toErrorsValidWedge
      )

  // Strips non-significant zeros on `reverseGet`
  val compactDecimalString: Format[String, String] =
    Format(
      _.some,
      s =>
        try BigDecimal(s).bigDecimal.stripTrailingZeros.toPlainString
        catch { case _ => s }
    )

  val compactDecimalStringValidWedge: InputValidWedge[String] =
    InputValidWedge.fromFormat(compactDecimalString)

  val pxValidWedge: InputValidWedge[Parallax] =
    compactDecimalStringValidWedge.andThen(
      InputValidWedge
        .truncatedBigDecimal(3.refined)
        .andThen(
          Parallax.milliarcseconds.reverse,
          NonEmptyChain(NonEmptyString.unsafeFrom("Invalid parallax"))
        )
    )

  val pmRAValidWedge: InputValidWedge[ProperMotion.RA] =
    compactDecimalStringValidWedge.andThen(
      InputValidWedge
        .truncatedBigDecimal(3.refined)
        .andThen(
          ProperMotion.RA.milliarcsecondsPerYear.reverse,
          NonEmptyChain(NonEmptyString.unsafeFrom("Invalid right ascencion velocity"))
        )
    )

  val pmDecValidWedge: InputValidWedge[ProperMotion.Dec] =
    compactDecimalStringValidWedge.andThen(
      InputValidWedge
        .truncatedBigDecimal(3.refined)
        .andThen(
          ProperMotion.Dec.milliarcsecondsPerYear.reverse,
          NonEmptyChain(NonEmptyString.unsafeFrom("Invalid declination velocity"))
        )
    )
