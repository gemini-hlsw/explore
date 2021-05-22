// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.syntax.all._
import coulomb.Quantity
import eu.timepit.refined.types.numeric.PosBigDecimal
import explore.model.enum.ImagingCapabilities
import lucuma.core.enum.FilterType
import lucuma.core.enum.GmosNorthFilter
import lucuma.core.enum.GmosSouthFilter
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.core.math.units._

import monocle.macros.Lenses
import spire.math.interval.ValueBound

sealed abstract class AvailableFilter {
  val tag: String
  val filterType: FilterType
  val shortName: String
  val centralWavelength: Wavelength
  val range: Option[Quantity[Int, Nanometer]]
}

@Lenses
final case class ImagingConfigurationOptions(
  filters:       Set[AvailableFilter],
  fov:           Option[Angle],
  signalToNoise: Option[PosBigDecimal],
  capabilities:  Option[ImagingCapabilities]
)

object ImagingConfigurationOptions {
  val Default = ImagingConfigurationOptions(Set.empty, none, none, none)

  val gmosNorthFilters = GmosNorthFilter.all
    .filterNot(f =>
      f.obsolete || f.filterType === FilterType.Spectroscopic || f.filterType === FilterType.Engineering
    )
    .map { f =>
      val l: Option[Wavelength] = f.width.map(_.lowerBound).collect { case ValueBound(a) => a }
      val u: Option[Wavelength] = f.width.map(_.upperBound).collect { case ValueBound(a) => a }
      new AvailableFilter {
        val filterType        = f.filterType
        val tag               = f.tag
        val shortName         = f.shortName
        val centralWavelength = f.wavelength
        val range             = (u, l).mapN(_.nanometer - _.nanometer)
      }
    }
  val gmosSouthFilters = GmosSouthFilter.all
    .filterNot(f =>
      f.obsolete || f.filterType === FilterType.Spectroscopic || f.filterType === FilterType.Engineering
    )
    .map { f =>
      val l: Option[Wavelength] = f.width.map(_.lowerBound).collect { case ValueBound(a) => a }
      val u: Option[Wavelength] = f.width.map(_.upperBound).collect { case ValueBound(a) => a }
      new AvailableFilter {
        val filterType        = f.filterType
        val tag               = f.tag
        val shortName: String = f.shortName
        val centralWavelength = f.wavelength
        val range             = (u, l).mapN(_.nanometer - _.nanometer)
      }
    }

  val availableOptions: List[AvailableFilter] =
    (gmosNorthFilters ++ gmosSouthFilters).distinctBy(_.tag)
}
