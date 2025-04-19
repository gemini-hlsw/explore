// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.itc

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import eu.timepit.refined.cats.given
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enums.Band
import lucuma.core.model.SourceProfile
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.SpectralDefinition.BandNormalized
import lucuma.core.model.SpectralDefinition.EmissionLines
import lucuma.itc.client.TargetInput

// From lucuma-odb, move to core
private val gaiaBands: Set[Band] =
  Set(Band.Gaia, Band.GaiaBP, Band.GaiaRP)

extension (self: SourceProfile)
  // Remove GAIA bands until the ITC supports them.
  def gaiaFree: SourceProfile =
    SourceProfile.integratedBrightnesses
      .modifyOption(_.removedAll(gaiaBands))(self)
      .orElse(
        SourceProfile.surfaceBrightnesses
          .modifyOption(_.removedAll(gaiaBands))(self)
      )
      .getOrElse(self)

case class ItcTarget(name: NonEmptyString, input: TargetInput) derives Eq:
  export input.*

  private def canQuerySD[A](sd: SpectralDefinition[A]): Boolean =
    sd match
      case BandNormalized(sed, bands) => sed.isDefined && bands.nonEmpty
      case EmissionLines(lines, _)    => lines.nonEmpty

  def canQueryITC: Boolean = input.sourceProfile match
    case SourceProfile.Point(sd)       => canQuerySD(sd)
    case SourceProfile.Uniform(sd)     => canQuerySD(sd)
    case SourceProfile.Gaussian(_, sd) => canQuerySD(sd)

  def gaiaFree: ItcTarget = copy(input = input.copy(sourceProfile = input.sourceProfile.gaiaFree))
