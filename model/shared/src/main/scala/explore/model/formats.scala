// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.syntax.all._
import coulomb._
import eu.timepit.refined.refineV
import explore.optics._
import lucuma.core.math.HourAngle.HMS
import lucuma.core.math.Parallax
import lucuma.core.math.ProperMotion.AngularVelocityComponent
import lucuma.core.math._
import lucuma.core.math.units._
import lucuma.core.optics._
import lucuma.core.syntax.string._

import java.text.NumberFormat
import java.util.Locale
import scala.math._

trait formats {
  val pxFormat: Format[String, Parallax] =
    Format(
      _.parseRationalOption
        .flatMap { r =>
          val micro = r * 1000L
          // isValidLong will also make sure there aren't too many decimals
          if (micro.isValidLong) micro.toLong.some else none
        }
        .flatMap(l => refineV[Parallax.Parallaxμas](l).toOption)
        .map(μas => Parallax(μas.withUnit[MicroArcSecond])),
      _.mas.to[BigDecimal, MilliArcSecond].value.toString
    )

  private def angularVelocityFormat[A](
    reverseGet: BigDecimal => AngularVelocityComponent[A]
  ): Format[String, AngularVelocityComponent[A]] =
    Format(_.parseBigDecimalOption.map(reverseGet),
           _.masy.to[BigDecimal, MilliArcSecondPerYear].value.toString
    )

  val pmRAFormat: Format[String, ProperMotion.RA] = angularVelocityFormat(
    ProperMotion.RA.milliarcsecondsPerYear.reverseGet
  )

  val pmDecFormat: Format[String, ProperMotion.Dec] = angularVelocityFormat(
    ProperMotion.Dec.milliarcsecondsPerYear.reverseGet
  )

  def formatterZ(dig: Int) = {
    val fmt = NumberFormat.getInstance(Locale.US)
    fmt.setGroupingUsed(false)
    fmt.setMaximumFractionDigits(scala.math.max(3, dig + 3))
    fmt
  }

  val formatterRV = {
    val fmt = NumberFormat.getInstance(Locale.US)
    fmt.setGroupingUsed(false)
    fmt.setMaximumFractionDigits(3)
    fmt
  }

  val formatterCZ = {
    val fmt = NumberFormat.getInstance(Locale.US)
    fmt.setGroupingUsed(false)
    fmt.setMaximumFractionDigits(3)
    fmt
  }

  val formatBigDecimalZ: Format[String, BigDecimal] =
    Format(_.parseBigDecimalOption, z => formatterZ(z.scale - z.precision).format(z))

  val formatBigDecimalCZ: Format[String, BigDecimal] =
    Format(_.parseBigDecimalOption, formatterCZ.format)

  val formatBigDecimalRV: Format[String, BigDecimal] =
    Format(_.parseBigDecimalOption, rv => formatterRV.format(rv))

  val formatRV: Format[String, RadialVelocity] =
    formatBigDecimalRV.andThen(fromKilometersPerSecondRV)

  val formatZ: Format[String, Redshift] =
    formatBigDecimalZ.andThen(redshiftBigDecimalIso)

  val formatCZ: Format[String, ApparentRadialVelocity] =
    formatBigDecimalCZ.andThen(fromKilometersPerSecondCZ)

  val formatWavelengthMicron: Format[String, Wavelength] =
    Format(_.parseBigDecimalOption.flatMap(Wavelength.decimalMicrometers.getOption),
           _.micrometer.to[BigDecimal, Micrometer].value.toString
    )

  // Note this format operates on Quantity to allow 0 values
  val formatMicron: Format[String, Quantity[BigDecimal, Micrometer]] =
    Format[String, Quantity[BigDecimal, Micrometer]](
      b => b.parseBigDecimalOption.filter(_ >= 0).map(_.withUnit[Micrometer]),
      w => w.value.toString
    )

  val formatArcsec: Format[String, Angle] =
    Format(_.parseIntOption.map(Angle.arcseconds.reverseGet(_)), Angle.arcseconds.get(_).toString)

  def formatHMS(hms: HMS): String =
    f"${hms.hours}%02d:${hms.minutes}%02d:${hms.seconds}%02d.${hms.milliseconds}%03d"

  val fromStringDMS: Angle => String =
    dms => {
      val r = Angle.dms.get(dms)
      f"${r.degrees}%02d:${r.arcminutes}%02d:${r.arcseconds}%02d.${r.milliarcseconds / 10}%02d"
    }

  val fromStringSignedDMS: Angle => String =
    a =>
      if (Angle.signedMicroarcseconds.get(a) < 0) s"-${fromStringDMS(-a)}"
      else s"+${fromStringDMS(a)}"

  def formatCoordinates(coords: Coordinates): String = {
    val ra = HMS(coords.ra.toHourAngle)
    s"${formatHMS(ra)} ${fromStringSignedDMS(coords.dec.toAngle)}"
  }

  def formatFov(angle: Angle): String = {
    val dms        = Angle.DMS(angle)
    val degrees    = dms.degrees
    val arcminutes = dms.arcminutes
    val arcseconds = dms.arcseconds
    val mas        = rint(dms.milliarcseconds.toDouble / 10).toInt
    if (degrees >= 45)
      f"$degrees%02d°"
    else if (degrees >= 1)
      f"$degrees%02d°$arcminutes%02d′"
    else if (arcminutes >= 1)
      f"$arcminutes%02d′$arcseconds%01d″"
    else
      f"$arcseconds%01d.$mas%02d″"
  }

}

object formats extends formats
