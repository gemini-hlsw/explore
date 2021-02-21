// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.syntax.all._
import coulomb._
import eu.timepit.refined.refineV
import explore.optics._
import lucuma.core.math.Parallax
import lucuma.core.math.ProperMotion.AngularVelocityComponent
import lucuma.core.math._
import lucuma.core.math.units._
import lucuma.core.optics.Format
import lucuma.core.syntax.string._

import java.text.NumberFormat
import java.util.Locale

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
    formatBigDecimalRV.composePrism(fromKilometersPerSecondRV)

  val formatZ: Format[String, Redshift] =
    formatBigDecimalZ.composeIso(redshiftBigDecimalISO)

  val formatCZ: Format[String, ApparentRadialVelocity] =
    formatBigDecimalCZ.composeIso(fromKilometersPerSecondCZ)
}

object formats extends formats
