// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.syntax.all._
import eu.timepit.refined.cats._
import explore.model.enum._
import lucuma.core.enum._
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ElevationRange
import lucuma.core.util.Display

import java.math.RoundingMode
import java.text.DecimalFormat

object display {
  implicit val displayProposalClass: Display[ProposalClass] =
    Display.byShortName(_.label)

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
  // Scientific notation only kicks in when more than 3 decimal digits would be needed.
  val displayBrightness: Display[BigDecimal] = Display.byShortName { x =>
    if (x.scale <= 3 && x.precision <= 5)
      x.toString
    else {
      // TODO The same scientific notation formatting is also defined in lucuma-ui.
      // Should we move it to a Format[String, BigDecimal] in lucuma-core?
      val formatter = new DecimalFormat("0.0E0")
      formatter.setRoundingMode(RoundingMode.HALF_UP)
      formatter.setMinimumFractionDigits(if (x.scale > 0) x.precision - 1 else x.scale)
      formatter.format(x.bigDecimal).stripSuffix("E0").toLowerCase
    }
  }
}
