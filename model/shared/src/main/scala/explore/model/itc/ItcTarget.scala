// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.itc

import cats.Hash
import cats.Order.given
import cats.syntax.all.*
import coulomb.syntax.*
import eu.timepit.refined.cats.given
import eu.timepit.refined.types.string.NonEmptyString
import explore.model.itc.math.*
import lucuma.core.enums.Band
import lucuma.core.math.BrightnessValue
import lucuma.core.math.RadialVelocity
import lucuma.core.math.Wavelength
import lucuma.core.model.SourceProfile

case class ItcTarget(name: NonEmptyString, rv: RadialVelocity, profile: SourceProfile)

object ItcTarget:
  extension (target: ItcTarget)
    def brightnessNearestTo(w: Wavelength): Option[(Band, BrightnessValue)] =
      val integrated = SourceProfile.integratedBrightnesses
        .getOption(target.profile)
        .flatMap { sb =>
          sb.minByOption { case (b, _) => (b.center.pm - w.pm).abs }
        }
        .map((b, v) => (b, v.value))
      val surface    = SourceProfile.surfaceBrightnesses
        .getOption(target.profile)
        .flatMap { sb =>
          sb.minByOption { case (b, _) => (b.center.pm - w.pm).abs }
        }
        .map((b, v) => (b, v.value))
      integrated.orElse(surface)

  extension (targets: List[ItcTarget])
    def brightestAt(wv: Wavelength): Option[ItcTarget] =
      targets.minByOption(_.brightnessNearestTo(wv).map(_._2))

  // We may consider adjusting this to consider small variations of RV identical for the
  // purpose of doing ITC calculations
  given Hash[RadialVelocity] = Hash.by(_.rv.value)
  given Hash[Band]           = Hash.by(_.tag)
  given Hash[SourceProfile]  = Hash.fromUniversalHashCode
  given Hash[ItcTarget]      = Hash.by(x => (x.name.value, x.rv, x.profile))
