// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.itc.math

import cats.data.NonEmptyList
import cats.syntax.all._
import explore.model.itc.ItcCcd
import lucuma.core.enums.Band
import lucuma.core.math.BrightnessUnits.*
import lucuma.core.math.Wavelength
import lucuma.core.model.SourceProfile
import lucuma.core.model.SpectralDefinition

import scala.math._

extension (ccds: NonEmptyList[ItcCcd])
  def maxPeakPixelFlux: Int    = ccds.maximumBy(_.peakPixelFlux).peakPixelFlux.toInt
  def maxSingleSNRatio: Double = ccds.maximumBy(_.singleSNRatio).singleSNRatio
  def maxTotalSNRatio: Double  = ccds.maximumBy(_.totalSNRatio).totalSNRatio
  def maxADU: Int              = ccds.maximumBy(_.adu).adu

def roundToSignificantFigures(num: Double, n: Int): Double =
  if num == 0 then 0
  else
    val d     = ceil(log10(abs(num)))
    val power = n - d.toInt

    val magnitude = pow(10, power)
    val shifted   = round(num * magnitude)
    shifted / magnitude

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

// Find the magnitude closest to the requested wavelength
def selectedBrightness(
  sourceProfile: SourceProfile,
  wavelength:    Wavelength
): Option[Band] =
  SourceProfile.integratedBandNormalizedSpectralDefinition
    .andThen(
      SpectralDefinition.BandNormalized.brightnesses[Integrated]
    )
    .getOption(sourceProfile)
    .orElse {
      SourceProfile.surfaceBandNormalizedSpectralDefinition
        .andThen(
          SpectralDefinition.BandNormalized.brightnesses[Surface]
        )
        .getOption(sourceProfile)
    }
    .map(_.keys)
    .traverse(
      _.minByOption((band: Band) => (band.center.pm - wavelength.pm).abs)
    )
    .collect { case Some(b) => b }
