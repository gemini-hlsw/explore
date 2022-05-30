// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma

import cats.syntax.all._
import lucuma.core.enum.ImageQuality
import lucuma.core.math.Wavelength
import coulomb._
import coulomb.refined._

import scala.math
import spire.math.Rational
import coulomb.accepted.ArcSecond
import lucuma.core.enum.GuideSpeed
import lucuma.core.enum.SkyBackground
import lucuma.core.enum.CloudExtinction
import lucuma.catalog.FaintnessConstraint
import lucuma.catalog.BrightnessConstraints
import lucuma.catalog.BandsList
import lucuma.catalog.SaturationConstraint
import lucuma.core.model.ConstraintSet

package object ags {
  val baseFwhm = Wavelength.fromNanometers(500).get

  // FWHM as seen on the optical wavefront sensor (WFS)
  // Operate on Double, we don't need exact precision
  def wfsFwhm(sciFwhm: ImageQuality, wavelength: Wavelength): Quantity[Double, ArcSecond] = {
    val coeff =
      baseFwhm.toPicometers.toValue[Rational] / wavelength.toPicometers.toValue[Rational]
    (sciFwhm.toArcSeconds * math.pow(coeff.value.toDouble, -0.2).withUnit[Unitless]).toValue[Double]

  }

  /**
   * Calculates the daintness limits for a given Guide Speed/Wavelength and conditions These are
   * based on the gaia G band and described here:
   */
  def faintLimit(
    guideSpeed: GuideSpeed,
    wavelength: Wavelength,
    sb:         SkyBackground,
    iq:         ImageQuality,
    ce:         CloudExtinction
  ): FaintnessConstraint = {
    val limit = sb match {
      case SkyBackground.Darkest =>
        guideSpeed match {
          case GuideSpeed.Fast   =>
            16.4 - 0.8 * wfsFwhm(iq, wavelength).value - ce.toBrightness
          case GuideSpeed.Medium =>
            16.9 - 0.8 * wfsFwhm(iq, wavelength).value - ce.toBrightness
          case GuideSpeed.Slow   =>
            17.4 - 0.8 * wfsFwhm(iq, wavelength).value - ce.toBrightness
        }
      case SkyBackground.Dark    =>
        guideSpeed match {
          case GuideSpeed.Fast   =>
            16.3 - 0.8 * wfsFwhm(iq, wavelength).value - ce.toBrightness
          case GuideSpeed.Medium =>
            16.8 - 0.8 * wfsFwhm(iq, wavelength).value - ce.toBrightness
          case GuideSpeed.Slow   =>
            17.3 - 0.8 * wfsFwhm(iq, wavelength).value - ce.toBrightness
        }
      case SkyBackground.Gray    =>
        guideSpeed match {
          case GuideSpeed.Fast   =>
            16.2 - 0.8 * wfsFwhm(iq, wavelength).value - ce.toBrightness
          case GuideSpeed.Medium =>
            16.7 - 0.8 * wfsFwhm(iq, wavelength).value - ce.toBrightness
          case GuideSpeed.Slow   =>
            17.2 - 0.8 * wfsFwhm(iq, wavelength).value - ce.toBrightness
        }
      case SkyBackground.Bright  =>
        guideSpeed match {
          case GuideSpeed.Fast   =>
            16.1 - 0.8 * wfsFwhm(iq, wavelength).value - ce.toBrightness
          case GuideSpeed.Medium =>
            16.6 - 0.8 * wfsFwhm(iq, wavelength).value - ce.toBrightness
          case GuideSpeed.Slow   =>
            17.1 - 0.8 * wfsFwhm(iq, wavelength).value - ce.toBrightness
        }
    }
    FaintnessConstraint(limit)
  }

  def gaiaBrightnessConstraints(
    guideSpeed: GuideSpeed,
    wavelength: Wavelength,
    sb:         SkyBackground,
    iq:         ImageQuality,
    ce:         CloudExtinction
  ): BrightnessConstraints = {
    val faintness  = faintLimit(guideSpeed, wavelength, sb, iq, ce)
    val saturation = SaturationConstraint(faintness.brightness - 6)
    BrightnessConstraints(BandsList.GaiaBandsList, faintness, saturation.some)
  }

  def gaiaBrightnessConstraints(
    constraints: ConstraintSet,
    guideSpeed:  GuideSpeed,
    wavelength:  Wavelength
  ): BrightnessConstraints =
    gaiaBrightnessConstraints(guideSpeed,
                              wavelength,
                              constraints.skyBackground,
                              constraints.imageQuality,
                              constraints.cloudExtinction
    )

}
