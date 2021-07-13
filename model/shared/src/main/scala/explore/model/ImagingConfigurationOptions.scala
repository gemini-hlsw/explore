// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Order
import cats.implicits._
import coulomb.Quantity
import eu.timepit.refined.types.numeric.PosBigDecimal
import explore.model.enum.ImagingCapabilities
import lucuma.core.enum.FilterType
import lucuma.core.enum.GmosNorthFilter
import lucuma.core.enum.GmosSouthFilter
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.core.math.units._
import monocle.Focus
import spire.math.interval.ValueBound

import scala.collection.SortedSet

sealed abstract class AvailableFilter {
  val tag: String
  val filterType: FilterType
  val shortName: String
  val centralWavelength: Wavelength
  val range: Option[Quantity[Int, Nanometer]]
}

object AvailableFilter {
  implicit val order: Order[AvailableFilter] = Order.by(x => (x.centralWavelength, x.tag))
}

final case class ImagingConfigurationOptions(
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
