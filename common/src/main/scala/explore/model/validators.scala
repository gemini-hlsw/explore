// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import lucuma.ui.optics.ValidFormatInput
import cats.data.Validated
import cats.data.NonEmptyChain
import explore.model.TruncatedAngle
import lucuma.core.math.Angle
import eu.timepit.refined.auto._
import cats.data.NonEmptyList
import eu.timepit.refined.types.string.NonEmptyString
import cats.syntax.all._
import scala.util.Try
import explore.model.display._
import lucuma.core.math.Offset
import lucuma.core.math.Axis
import explore.optics._
import explore.model.DitherNanoMeters
import explore.model.DitherNanoMetersRange

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
