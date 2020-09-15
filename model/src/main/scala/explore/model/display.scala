// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import explore.model.enum._
import lucuma.core.util.Display

object display {
  implicit val displaySkyBackground: Display[SkyBackground] =
    Display.byShortName(_.label)

  implicit val displayWaterVapor: Display[WaterVapor] =
    Display.byShortName(_.label)

  implicit val displayCloudCover: Display[CloudCover] =
    Display.byShortName(_.label)

  implicit val displayImageQuality: Display[ImageQuality] =
    Display.byShortName(_.label)

  implicit val displayProposalClass: Display[ProposalClass] =
    Display.byShortName(_.label)

  implicit val displayKeywords: Display[Keyword] =
    Display.byShortName(_.label)

  implicit val displayTacCategory: Display[TacCategory] =
    Display.byShortName(_.label)

  implicit val displayToOActivation: Display[ToOActivation] =
    Display.byShortName(_.label)
}
