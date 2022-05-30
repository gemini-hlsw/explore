// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ags

import cats.Eq
import cats.syntax.all._
import coulomb._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enum.Band
import lucuma.core.enum.StellarLibrarySpectrum
import lucuma.core.math.BrightnessUnits._
import lucuma.core.math.dimensional._
import lucuma.core.math.units._
import lucuma.core.model.SiderealTracking
import lucuma.core.model.SourceProfile
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.Target
import lucuma.core.model.UnnormalizedSED
import lucuma.core.optics.SplitEpi

import scala.collection.immutable.SortedMap

/**
 * Poors' man Target.Sidereal with a single G brightness and no extra metadata
 */
final case class GuideStarCandidate(
  name:        NonEmptyString,
  tracking:    SiderealTracking,
  gBrightness: Option[BigDecimal]
)

object GuideStarCandidate {
  implicit val eqGuideStar: Eq[GuideStarCandidate] = Eq.by(x => (x.name, x.tracking, x.gBrightness))

  val siderealTarget: SplitEpi[Target.Sidereal, GuideStarCandidate] =
    SplitEpi(
      st =>
        GuideStarCandidate(
          st.name,
          st.tracking,
          SourceProfile.integratedBrightnessIn(Band.Gaia).headOption(st.sourceProfile).map(_.value)
        ),
      g =>
        Target.Sidereal(
          g.name,
          g.tracking,
          SourceProfile.Point(
            SpectralDefinition.BandNormalized(
              UnnormalizedSED.StellarLibrary(StellarLibrarySpectrum.O5V),
              SortedMap.from(
                g.gBrightness
                  .foldMap(g => List(Band.Gaia -> g.withUnit[VegaMagnitude].toMeasureTagged))
                  .toSeq
              )
            )
          ),
          none
        )
    )
}
