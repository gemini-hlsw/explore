// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import explore.model.enum._
import lucuma.core.enum._
import lucuma.core.util.Display

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

}
