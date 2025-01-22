// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.modes

import cats.Eq
import cats.derived.*
import cats.implicits.*
import eu.timepit.refined.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.string.*
import lucuma.core.enums.*
import lucuma.core.math.Wavelength
import lucuma.core.math.units.*
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.schemas.model.CentralWavelength
import monocle.Getter

sealed trait InstrumentConfig derives Eq:
  def instrument: Instrument

  type Grating
  val grating: Grating
  def gratingDisplay: Display[Grating]
  def gratingStr: String = gratingDisplay.shortName(grating)

  type FPU
  val fpu: FPU

  type Filter
  val filter: Filter

  val site: Site

  def hasFilter: Boolean

  type Override
  def modeOverrides: Option[Override] = None

object InstrumentConfig:
  case class GmosNorthSpectroscopy(
    grating:                    GmosNorthGrating,
    fpu:                        GmosNorthFpu,
    filter:                     Option[GmosNorthFilter],
    override val modeOverrides: Option[InstrumentOverrides.GmosSpectroscopy]
  ) extends InstrumentConfig derives Eq {
    type Grating  = GmosNorthGrating
    type Filter   = Option[GmosNorthFilter]
    type FPU      = GmosNorthFpu
    type Override = InstrumentOverrides.GmosSpectroscopy

    val gratingDisplay: Display[Grating] = Display.byShortName(_.shortName)
    val instrument                       = Instrument.GmosNorth
    val site                             = Site.GN
    val hasFilter                        = filter.isDefined
  }

  case class GmosSouthSpectroscopy(
    grating:                    GmosSouthGrating,
    fpu:                        GmosSouthFpu,
    filter:                     Option[GmosSouthFilter],
    override val modeOverrides: Option[InstrumentOverrides.GmosSpectroscopy]
  ) extends InstrumentConfig derives Eq {
    type Grating  = GmosSouthGrating
    type Filter   = Option[GmosSouthFilter]
    type FPU      = GmosSouthFpu
    type Override = InstrumentOverrides.GmosSpectroscopy
    val gratingDisplay: Display[Grating] = Display.byShortName(_.shortName)
    val instrument                       = Instrument.GmosSouth
    val site                             = Site.GS
    val hasFilter                        = filter.isDefined
  }

  case class Flamingos2Spectroscopy(grating: Option[F2Disperser], filter: F2Filter, fpu: F2Fpu)
      extends InstrumentConfig derives Eq {
    type Grating  = Option[F2Disperser]
    type Filter   = F2Filter
    type FPU      = F2Fpu
    type Override = Unit
    val gratingDisplay: Display[Grating] =
      Display.byShortName(_.map(_.shortName).getOrElse("None"))
    val instrument                       = Instrument.Flamingos2
    val site                             = Site.GS
    val hasFilter                        = true
  }

  case class GpiSpectroscopy(grating: GpiDisperser, filter: GpiFilter) extends InstrumentConfig
      derives Eq {
    type Grating  = GpiDisperser
    type Filter   = GpiFilter
    type FPU      = Unit
    type Override = Unit
    val gratingDisplay: Display[Grating] = Display.byShortName(_.shortName)
    val fpu                              = ()
    val instrument                       = Instrument.Gpi
    val site                             = Site.GN
    val hasFilter                        = true
  }

  case class GnirsSpectroscopy(grating: GnirsDisperser, filter: GnirsFilter)
      extends InstrumentConfig derives Eq {
    type Grating  = GnirsDisperser
    type Filter   = GnirsFilter
    type FPU      = Unit
    type Override = Unit
    val gratingDisplay: Display[Grating] = Display.byShortName(_.shortName)
    val fpu                              = ()
    val instrument                       = Instrument.Gnirs
    val site                             = Site.GN
    val hasFilter                        = true
  }

  // Used for Instruments not fully defined
  case class GenericSpectroscopy(i: Instrument, grating: String, filter: NonEmptyString)
      extends InstrumentConfig derives Eq {
    type Grating  = String
    type Filter   = NonEmptyString
    type FPU      = Unit
    type Override = Unit
    val gratingDisplay: Display[Grating] = Display.byShortName(identity)
    val fpu                              = ()
    val instrument                       = i
    val site                             = Site.GN
    val hasFilter                        = true
  }

  val instrument: Getter[InstrumentConfig, Instrument] =
    Getter[InstrumentConfig, Instrument](_.instrument)

  def grating: Getter[InstrumentConfig, InstrumentConfig#Grating] =
    Getter[InstrumentConfig, InstrumentConfig#Grating](_.grating)

  def filter: Getter[InstrumentConfig, InstrumentConfig#Filter] =
    Getter[InstrumentConfig, InstrumentConfig#Filter](_.filter)
