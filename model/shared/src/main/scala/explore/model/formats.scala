// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.syntax.all.*
import coulomb.*
import coulomb.ops.algebra.spire.all.given
import coulomb.policy.spire.standard.given
import coulomb.syntax.*
import eu.timepit.refined.refineV
import explore.optics.all.*
import lucuma.core.math.HourAngle.HMS
import lucuma.core.math.Parallax
import lucuma.core.math.ProperMotion.AngularVelocityComponent
import lucuma.core.math.*
import lucuma.core.math.units.*
import lucuma.core.optics.*
import lucuma.core.syntax.string.*
import spire.math.Rational

import java.text.NumberFormat
import java.util.Locale
import scala.math.*

trait formats:
  private def formatterZ(dig: Int): NumberFormat = {
    val fmt = NumberFormat.getInstance(Locale.US)
    fmt.setGroupingUsed(false)
    fmt.setMaximumFractionDigits(scala.math.max(3, dig + 3))
    fmt
  }

  private val formatterRV: NumberFormat = {
    val fmt = NumberFormat.getInstance(Locale.US)
    fmt.setGroupingUsed(false)
    fmt.setMaximumFractionDigits(3)
    fmt
  }

  private val formatterCZ: NumberFormat = {
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
           _.micrometer.toUnit[Micrometer].value.toString
    )

  val formatArcsec: Format[String, Angle] =
    Format(_.parseIntOption.map(Angle.arcseconds.reverseGet(_)), Angle.arcseconds.get(_).toString)

  private def formatHMS(hms: HMS): String =
    f"${hms.hours}%02d:${hms.minutes}%02d:${hms.seconds}%02d.${hms.milliseconds}%03d"

  private val fromStringDMS: Angle => String =
    dms => {
      val r = Angle.dms.get(dms)
      f"${r.degrees}%02d:${r.arcminutes}%02d:${r.arcseconds}%02d.${r.milliarcseconds / 10}%02d"
    }

  private val fromStringSignedDMS: Angle => String =
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

object formats extends formats
