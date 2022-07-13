// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.data.NonEmptyList
import cats.syntax.all._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.auto._
import eu.timepit.refined.types.string.NonEmptyString
import explore.model.DitherNanoMeters
import explore.model.DitherNanoMetersRange
import explore.model.HourRange
import explore.model.display._
import lucuma.core.math.Axis
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.core.math.validation.MathValidators
import lucuma.core.optics.ValidSplitEpi
import lucuma.core.optics.ValidWedge
import lucuma.core.validation._

import scala.util.Try

object ExploreModelValidators {
  val brightnessValidWedge: InputValidWedge[BigDecimal] =
    InputValidWedge(
      InputValidSplitEpi.bigDecimal.withErrorMessage("Invalid brightness value").getValid,
      n => Try(displayBrightness.shortName(n)).toOption.orEmpty
    )

  val dithersValidSplitEpi: InputValidSplitEpi[Option[NonEmptyList[DitherNanoMeters]]] =
    InputValidSplitEpi
      .refinedBigDecimal[DitherNanoMetersRange]
      .toNel()
      .withErrorMessage("Invalid wavelength dither values")
      .optional

  val offsetQNELValidWedge: InputValidWedge[Option[NonEmptyList[Offset.Q]]] =
    MathValidators.truncatedAngleSignedDegrees
      .andThen(Offset.Component.angle[Axis.Q].reverse)
      .toNel()
      .withErrorMessage("Invalid offsets")
      .optional

  val hoursValidWedge: InputValidWedge[BigDecimal Refined HourRange] =
    InputValidWedge
      .truncatedBigDecimal(decimals = 2)
      .andThen(
        ValidSplitEpi
          .forRefined[String, BigDecimal, HourRange]("Invalid hour value")
          .toErrorsValidSplitEpiUnsafe
      )

  val wavelengthValidWedge: InputValidWedge[Wavelength] =
    InputValidWedge
      .truncatedBigDecimal(3)
      .andThen(
        ValidWedge
          .fromFormat(Wavelength.decimalMicrometers,
                      NonEmptyString.unsafeFrom("Invalid Wavelength")
          )
          .toErrorsValidWedge
      )
}
