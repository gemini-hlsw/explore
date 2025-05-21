// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
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
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.schemas.model.CentralWavelength
import monocle.Getter

sealed trait ItcInstrumentConfig derives Eq:
  def instrument: Instrument

  type Grating
  val grating: Grating
  def gratingDisplay: Display[Grating]
  def gratingStr: String = gratingDisplay.shortName(grating)
  def filterStr: String

  type FPU
  val fpu: FPU

  type Filter
  val filter: Filter

  val site: Site

  def hasFilter: Boolean

  val mode: ScienceMode

  type Override
  def modeOverrides: Option[Override] = None

object ItcInstrumentConfig:
  // GMOS suporta a total wavelength range of 360-1030 nm
  // https://www.gemini.edu/instrumentation/gmos
  // the center is 360 + (1030 - 360) / 2 = 695
  val GmosFallbackCW: CentralWavelength =
    CentralWavelength(Wavelength.fromIntNanometers(695).get)

  case class GmosNorthSpectroscopy(
    grating:                    GmosNorthGrating,
    fpu:                        GmosNorthFpu,
    filter:                     Option[GmosNorthFilter],
    override val modeOverrides: Option[InstrumentOverrides.GmosSpectroscopy]
  ) extends ItcInstrumentConfig derives Eq {
    type Grating  = GmosNorthGrating
    type Filter   = Option[GmosNorthFilter]
    type FPU      = GmosNorthFpu
    type Override = InstrumentOverrides.GmosSpectroscopy

    val gratingDisplay: Display[Grating] = Display.byShortName(_.shortName)
    val filterStr: String                = filter.fold("none")(_.shortName)
    val instrument                       = Instrument.GmosNorth
    val site                             = Site.GN
    val hasFilter                        = filter.isDefined
    val mode                             = ScienceMode.Spectroscopy
  }

  case class GmosSouthSpectroscopy(
    grating:                    GmosSouthGrating,
    fpu:                        GmosSouthFpu,
    filter:                     Option[GmosSouthFilter],
    override val modeOverrides: Option[InstrumentOverrides.GmosSpectroscopy]
  ) extends ItcInstrumentConfig derives Eq {
    type Grating  = GmosSouthGrating
    type Filter   = Option[GmosSouthFilter]
    type FPU      = GmosSouthFpu
    type Override = InstrumentOverrides.GmosSpectroscopy
    val gratingDisplay: Display[Grating] = Display.byShortName(_.shortName)
    val filterStr: String                = filter.fold("none")(_.shortName)
    val instrument                       = Instrument.GmosSouth
    val site                             = Site.GS
    val hasFilter                        = filter.isDefined
    val mode                             = ScienceMode.Spectroscopy
  }

  case class GmosNorthImaging(
    filter:                     GmosNorthFilter,
    override val modeOverrides: Option[InstrumentOverrides.GmosImaging]
  ) extends ItcInstrumentConfig derives Eq {
    type Grating  = Unit
    type Filter   = GmosNorthFilter
    type FPU      = Unit
    type Override = InstrumentOverrides.GmosImaging

    val gratingDisplay: Display[Grating] = Display.byShortName(_ => "")
    val filterStr: String                = filter.shortName
    val instrument                       = Instrument.GmosNorth
    val site                             = Site.GN
    val hasFilter                        = true
    val mode                             = ScienceMode.Imaging
    val grating: Grating                 = ()
    val fpu: FPU                         = ()
  }

  case class GmosSouthImaging(
    filter:                     GmosSouthFilter,
    override val modeOverrides: Option[InstrumentOverrides.GmosImaging]
  ) extends ItcInstrumentConfig derives Eq {

    type Grating  = Unit
    type Filter   = GmosSouthFilter
    type FPU      = Unit
    type Override = InstrumentOverrides.GmosImaging
    val gratingDisplay: Display[Grating] = Display.byShortName(_ => "")
    val filterStr: String                = filter.shortName
    val instrument                       = Instrument.GmosSouth
    val site                             = Site.GS
    val hasFilter                        = true
    val mode                             = ScienceMode.Imaging
    val grating: Grating                 = ()
    val fpu: FPU                         = ()

  }

  case class Flamingos2Spectroscopy(
    grating: Flamingos2Disperser,
    filter:  Flamingos2Filter,
    fpu:     Flamingos2Fpu
  ) extends ItcInstrumentConfig derives Eq {
    type Grating  = Flamingos2Disperser
    type Filter   = Flamingos2Filter
    type FPU      = Flamingos2Fpu
    type Override = Unit
    val gratingDisplay: Display[Grating] = Display.byShortName(_.shortName)
    val filterStr: String                = filter.shortName
    val instrument                       = Instrument.Flamingos2
    val site                             = Site.GS
    val hasFilter                        = true
    val mode                             = ScienceMode.Spectroscopy

  }

  case class GpiSpectroscopy(grating: GpiDisperser, filter: GpiFilter) extends ItcInstrumentConfig
      derives Eq {
    type Grating  = GpiDisperser
    type Filter   = GpiFilter
    type FPU      = Unit
    type Override = Unit
    val gratingDisplay: Display[Grating] = Display.byShortName(_.shortName)
    val filterStr: String                = filter.shortName
    val fpu                              = ()
    val instrument                       = Instrument.Gpi
    val site                             = Site.GN
    val hasFilter                        = true
    val mode                             = ScienceMode.Spectroscopy
  }

  case class GnirsSpectroscopy(grating: GnirsDisperser, filter: GnirsFilter)
      extends ItcInstrumentConfig derives Eq {
    type Grating  = GnirsDisperser
    type Filter   = GnirsFilter
    type FPU      = Unit
    type Override = Unit
    val gratingDisplay: Display[Grating] = Display.byShortName(_.shortName)
    val filterStr: String                = filter.shortName
    val fpu                              = ()
    val instrument                       = Instrument.Gnirs
    val site                             = Site.GN
    val hasFilter                        = true
    val mode                             = ScienceMode.Spectroscopy
  }

  // Used for Instruments not fully defined
  case class GenericSpectroscopy(i: Instrument, grating: String, filter: NonEmptyString)
      extends ItcInstrumentConfig derives Eq {
    type Grating  = String
    type Filter   = NonEmptyString
    type FPU      = Unit
    type Override = Unit
    val gratingDisplay: Display[Grating] = Display.byShortName(identity)
    val filterStr: String                = filter.value
    val fpu                              = ()
    val instrument                       = i
    val site                             = Site.GN
    val hasFilter                        = true
    val mode                             = ScienceMode.Spectroscopy
  }

  val instrument: Getter[ItcInstrumentConfig, Instrument] =
    Getter[ItcInstrumentConfig, Instrument](_.instrument)

  def grating: Getter[ItcInstrumentConfig, ItcInstrumentConfig#Grating] =
    Getter[ItcInstrumentConfig, ItcInstrumentConfig#Grating](_.grating)

  def filter: Getter[ItcInstrumentConfig, ItcInstrumentConfig#Filter] =
    Getter[ItcInstrumentConfig, ItcInstrumentConfig#Filter](_.filter)
