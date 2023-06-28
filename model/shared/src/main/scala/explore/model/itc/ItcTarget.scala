// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.itc

import cats.Eq
import cats.Order.given
import cats.derived.*
import cats.syntax.all.*
import eu.timepit.refined.cats.given
import eu.timepit.refined.types.string.NonEmptyString
import explore.model.itc.math.*
import lucuma.core.enums.Band
import lucuma.core.math.BrightnessValue
import lucuma.core.math.RadialVelocity
import lucuma.core.math.Wavelength
import lucuma.core.math.dimensional.Units
import lucuma.core.model.SourceProfile
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.SpectralDefinition.BandNormalized
import lucuma.core.model.SpectralDefinition.EmissionLines

case class ItcTarget(name: NonEmptyString, rv: RadialVelocity, profile: SourceProfile) derives Eq:
  private def canQuerySD[A](sd: SpectralDefinition[A]): Boolean =
    sd match
      case BandNormalized(sed, _) => sed.isDefined
      case EmissionLines(_, _)    => false // No emission line support yet

  def canQueryITC: Boolean = profile match
    case SourceProfile.Point(sd)       => canQuerySD(sd)
    case SourceProfile.Uniform(sd)     => canQuerySD(sd)
    case SourceProfile.Gaussian(_, sd) => canQuerySD(sd)

object ItcTarget:
  extension (target: ItcTarget)
    def brightnessNearestTo(w: Wavelength): Option[(Band, BrightnessValue, Units)] =
      val integrated = SourceProfile.integratedBrightnesses
        .getOption(target.profile)
        .flatMap { sb =>
          sb.minByOption { case (b, _) => (b.center.pm - w.pm).abs }
        }
        .map((b, v) => (b, v.value, v.units))
      val surface    = SourceProfile.surfaceBrightnesses
        .getOption(target.profile)
        .flatMap { sb =>
          sb.minByOption { case (b, _) => (b.center.pm - w.pm).abs }
        }
        .map((b, v) => (b, v.value, v.units))
      integrated.orElse(surface)

  extension (targets: List[ItcTarget])
    def brightestAt(wv: Wavelength): Option[ItcTarget] =
      targets.minByOption(_.brightnessNearestTo(wv).map(_._2))
