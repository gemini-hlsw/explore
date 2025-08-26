// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.itc.math

import cats.data.NonEmptyChain
import cats.syntax.all.*
import lucuma.core.math.Wavelength
import lucuma.itc.ItcCcd
import lucuma.itc.SingleSN
import lucuma.itc.TotalSN

import scala.math.*

extension (ccds: NonEmptyChain[ItcCcd])
  def maxPeakPixelFlux: Int      = ccds.maximumBy(_.peakPixelFlux).peakPixelFlux.toInt
  def maxSingleSNRatio: SingleSN = ccds.maximumBy(_.singleSNRatio).singleSNRatio
  def maxTotalSNRatio: TotalSN   = ccds.maximumBy(_.totalSNRatio).totalSNRatio
  def maxADU: Int                = ccds.maximumBy(_.adu).adu

/**
 * Returns a "nice" number approximately equal to range Rounds the number if round = true Takes the
 * ceiling if round = false.
 */
def niceNum(range: Double, round: Boolean): Double =
  val exponent = floor(log10(range))
  val fraction = range / pow(10, exponent)

  val niceFraction =
    if round then
      fraction match
        case f if f < 1.5 => 1
        case f if f < 3   => 2
        case f if f < 7   => 5
        case _            => 10
    else
      fraction match
        case f if f <= 1 => 1
        case f if f <= 2 => 2
        case f if f <= 5 => 5
        case _           => 10

  niceFraction * pow(10, exponent)

extension (w: Wavelength) inline def pm = w.toPicometers.value.value
