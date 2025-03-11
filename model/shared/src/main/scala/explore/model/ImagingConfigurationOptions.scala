// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.Order
import cats.implicits.*
import coulomb.*
import coulomb.policy.spire.standard.given
import coulomb.units.si.prefixes.Nano
import eu.timepit.refined.types.numeric.PosBigDecimal
import explore.model.enums.ImagingCapabilities
import lucuma.core.enums.FilterType
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.core.math.units.*
import monocle.Focus
import spire.math.interval.ValueBound

import scala.collection.immutable.SortedSet

sealed abstract class AvailableFilter {
  val tag: String
  val filterType: FilterType
  val shortName: String
  val centralWavelength: Wavelength
  val range: Option[Quantity[Int, Nanometer]]
}

object AvailableFilter {
  given Order[AvailableFilter] = Order.by(x => (x.centralWavelength, x.tag))
}

case class ImagingConfigurationOptions(
  filters:       SortedSet[AvailableFilter],
  fov:           Option[Angle],
  signalToNoise: Option[PosBigDecimal],
  capabilities:  Option[ImagingCapabilities]
)

object ImagingConfigurationOptions {
  val filters       = Focus[ImagingConfigurationOptions](_.filters)
  val fov           = Focus[ImagingConfigurationOptions](_.fov)
  val signalToNoise = Focus[ImagingConfigurationOptions](_.signalToNoise)
  val capabilities  = Focus[ImagingConfigurationOptions](_.capabilities)

  val Default = ImagingConfigurationOptions(SortedSet.empty, none, none, none)

  val gmosNorthFilters = GmosNorthFilter.all
    .filterNot(f =>
      f.filterType === FilterType.Spectroscopic || f.filterType === FilterType.Engineering
    )
    .map { f =>
      val l: Option[Wavelength] = f.width.lowerBound match {
        case ValueBound(a) => a.some
        case _             => none
      }
      val u: Option[Wavelength] = f.width.upperBound match {
        case ValueBound(a) => a.some
        case _             => none
      }
      new AvailableFilter {
        val filterType        = f.filterType
        val tag               = f.tag
        val shortName         = f.shortName
        val centralWavelength = f.wavelength
        val range             = (u, l).mapN { (a, b) =>
          a.diff(b).toNanometers.tToValue[Int]
        }
      }
    }

  val gmosSouthFilters = GmosSouthFilter.all
    .filterNot(f =>
      f.filterType === FilterType.Spectroscopic || f.filterType === FilterType.Engineering
    )
    .map { f =>
      val l: Option[Wavelength] = f.width.lowerBound match {
        case ValueBound(a) => a.some
        case _             => none
      }
      val u: Option[Wavelength] = f.width.upperBound match {
        case ValueBound(a) => a.some
        case _             => none
      }
      new AvailableFilter {
        val filterType        = f.filterType
        val tag               = f.tag
        val shortName: String = f.shortName
        val centralWavelength = f.wavelength
        val range             = (u, l).mapN { (a, b) =>
          a.diff(b).toNanometers.tToValue[Int]
        }
      }
    }

  val availableOptions: List[AvailableFilter] =
    (gmosNorthFilters ++ gmosSouthFilters).distinctBy(_.tag)

  given Eq[ImagingConfigurationOptions] = Eq.fromUniversalEquals
}
