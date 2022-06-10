// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import cats.data.Validated
import cats.syntax.all._
import eu.timepit.refined.auto._
import eu.timepit.refined.types.string.NonEmptyString
import explore.model.DitherNanoMeters
import explore.model.DitherNanoMetersRange
import explore.model.TruncatedAngle
import explore.model.display._
import explore.optics._
import lucuma.core.math.Angle
import lucuma.core.math.Axis
import lucuma.core.math.Offset
import lucuma.ui.optics.ValidFormatInput

import scala.util.Try

object validators {
  val truncatedAngleValidFormat = ValidFormatInput[TruncatedAngle](
    s => {
      val ota = s.toDoubleOption
        .map(Angle.fromDoubleDegrees)
        .map(TruncatedAngle(_))
      Validated.fromOption(ota, NonEmptyChain("Invalid Position Angle"))
    },
    pa => f"${pa.angle.toSignedDoubleDegrees}%.2f"
  )

  // We can't define a Format[String, Angle] for arcseconds. Roundtrip laws fail because of rounding.
  val angleValidFormat: ValidFormatInput[Angle] =
    ValidFormatInput(
      s =>
        Validated.fromOption(
          (s.toDoubleOption.map(Angle.fromDoubleArcseconds)),
          NonEmptyChain("Invalid angle")
        ),
      a => (a.toMicroarcseconds / 1000000.0).toString
    )

  val brightnessValidFormat: ValidFormatInput[BigDecimal] =
    ValidFormatInput(
      ValidFormatInput.bigDecimalValidFormat("Invalid brightness value").getValidated,
      n => Try(displayBrightness.shortName(n)).toOption.orEmpty
    )

  val dithersValidFormat: ValidFormatInput[Option[NonEmptyList[DitherNanoMeters]]] =
    ValidFormatInput
      .forRefinedBigDecimal[DitherNanoMetersRange]()
      .toNel(error = NonEmptyString("Invalid wavelength dither values").some)
      .optional

  val offsetQNELValidFormat: ValidFormatInput[Option[NonEmptyList[Offset.Q]]] =
    truncatedAngleValidFormat
      .andThen(angleTruncatedAngleSplitEpi.reverse)
      .andThen(Offset.Component.angle[Axis.Q].reverse)
      .toNel(error = NonEmptyString("Invalid offsets").some)
      .optional
}
