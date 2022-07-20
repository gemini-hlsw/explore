// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.syntax.all._
import eu.timepit.refined.cats._
import lucuma.core.enums._
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ElevationRange
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.UnnormalizedSED
import lucuma.core.syntax.display._
import lucuma.core.util.Display
import lucuma.core.validation.InputValidSplitEpi

import java.text.DecimalFormat

trait DisplayImplicits {
  implicit val displayTacGroup: Display[TacGroup] =
    Display.byShortName(_.label)

  implicit val displayTacCategory: Display[TacCategory] =
    Display.byShortName(_.label)

  implicit val displayToOActivation: Display[ToOActivation] =
    Display.byShortName(_.label)

  implicit val displayBand: Display[Band] =
    Display.by(_.shortName, _.longName)

  implicit val spectroscopyCapabilitiesDisplay: Display[SpectroscopyCapabilities] =
    Display.byShortName {
      case SpectroscopyCapabilities.NodAndShuffle => "Nod & Shuffle"
      case SpectroscopyCapabilities.Polarimetry   => "Polarimetry"
      case SpectroscopyCapabilities.Coronagraphy  => "Coronagraphy"
    }

  implicit val focaLPlaneDisplay: Display[FocalPlane] = Display.byShortName {
    case FocalPlane.SingleSlit   => "Single Slit"
    case FocalPlane.MultipleSlit => "Multiple Slits"
    case FocalPlane.IFU          => "IFU"
  }

  implicit val scienceModeDisplay: Display[ScienceMode] = Display.byShortName {
    case ScienceMode.Imaging      => "Imaging"
    case ScienceMode.Spectroscopy => "Spectroscopy"
  }

  implicit val displayImageQuality: Display[ImageQuality] = Display.byShortName(_.label)

  implicit val displayCloudExtinction: Display[CloudExtinction] = Display.byShortName(_.label)

  implicit val displayWaterVapor: Display[WaterVapor] = Display.byShortName(_.label)

  implicit val displaySkyBackground: Display[SkyBackground] = Display.byShortName(_.label)

  implicit val displayConstraintSet: Display[ConstraintSet] = Display.byShortName { cs =>
    val wv = if (cs.waterVapor === WaterVapor.Wet) "" else s" ${cs.waterVapor.label}"
    val er = cs.elevationRange match {
      case ElevationRange.AirMass(min, max)
          if min === ElevationRange.AirMass.DefaultMin && max === ElevationRange.AirMass.DefaultMax =>
        ""
      case ElevationRange.AirMass(min, max) if min === ElevationRange.AirMass.DefaultMin     =>
        f" AM<${max.value}%.1f"
      case ElevationRange.AirMass(min, max) if max === ElevationRange.AirMass.DefaultMax     =>
        f" ${min.value}%.1f<AM"
      case ElevationRange.AirMass(min, max)                                                  =>
        f" ${min.value}%.1f<AM<${max.value}%.1f"
      case ElevationRange.HourAngle(min, max) if min === ElevationRange.HourAngle.DefaultMin =>
        f" HA<${max.value}%.1f"
      case ElevationRange.HourAngle(min, max) if max === ElevationRange.HourAngle.DefaultMax =>
        f" ${min.value}%.1f<HA"
      case ElevationRange.HourAngle(min, max)                                                =>
        f" ${min.value}%.1f<HA<${max.value}%.1f"
    }

    s"${cs.imageQuality.label} ${cs.cloudExtinction.label} ${cs.skyBackground.label}$wv$er"
  }

  // Not implicit. When we have opaque types we may define a BrightnessValue.
  val displayBrightness: Display[BigDecimal] = Display.byShortName { x =>
    val f = new DecimalFormat("#.###")

    // We don't want scientific notation to kick in for magnitude units.
    // We assume it's magnitudes when x.abs >= 0.00001, since other units are usually
    // expressed in the order of e-18 or e-19.
    // We could make the format depend on the units, but that may be confusing for users
    // in case they want to type the value and then change the units.
    if (x.abs >= 0.00001)
      f.format(x).replace("-0", "0")
    else
      InputValidSplitEpi.bigDecimalWithScientificNotation.reverseGet(x)
  }

  implicit val displayLibrarySpectrum: Display[StellarLibrarySpectrum] = Display.byTag

  import UnnormalizedSED._
  implicit val displayUnnormalizedSED: Display[UnnormalizedSED] = Display.byShortName {
    case StellarLibrary(librarySpectrum)          => librarySpectrum.shortName
    case CoolStarModel(temperature)               => s"Cool Star (${temperature.temperature.value} °K)"
    case Galaxy(galaxySpectrum)                   => galaxySpectrum.shortName
    case Planet(planetSpectrum)                   => planetSpectrum.shortName
    case Quasar(quasarSpectrum)                   => quasarSpectrum.shortName
    case HIIRegion(hiiRegionSpectrum)             => hiiRegionSpectrum.shortName
    case PlanetaryNebula(planetaryNebulaSpectrum) => planetaryNebulaSpectrum.shortName
    case PowerLaw(index)                          => s"Power Law ($index)"
    case BlackBody(temperature)                   => s"Black Body (${temperature.value} °K)"
    case UserDefined(_)                           => "User Defined"
  }

  implicit def displaySpectralDefinition[T]: Display[SpectralDefinition[T]] = Display.byShortName {
    case SpectralDefinition.BandNormalized(band, _) => band.shortName
    case SpectralDefinition.EmissionLines(_, _)     => "Emission Lines"
  }

  implicit val displayGmosXBinning: Display[GmosXBinning] = Display.byTag

  implicit val displayGmosYBinning: Display[GmosYBinning] = Display.byTag

  implicit val displayGmosNorthGrating: Display[GmosNorthGrating] = Display.byTag

  implicit val displayGmosSouthGrating: Display[GmosSouthGrating] = Display.byTag

  implicit val displayGmosNorthFilter: Display[GmosNorthFilter] = Display.byTag

  implicit val displayGmosSouthFilter: Display[GmosSouthFilter] = Display.byTag

  implicit val displayGmosNorthFpu: Display[GmosNorthFpu] = Display.byTag

  implicit val displayGmosSouthFpu: Display[GmosSouthFpu] = Display.byTag

  implicit val displayGmosAmpReadMode: Display[GmosAmpReadMode] = Display.byTag

  implicit val displayGmosAmpGain: Display[GmosAmpGain] = Display.byTag

  implicit val displayGmosRoi: Display[GmosRoi] = Display.byTag
}

object display extends DisplayImplicits
