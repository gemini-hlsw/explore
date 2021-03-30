// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import explore.model.enum._
import lucuma.core.enum.MagnitudeBand
import lucuma.core.enum.MagnitudeSystem
import lucuma.core.util.Display

object display {
  implicit val displayProposalClass: Display[ProposalClass] =
    Display.byShortName(_.label)

  implicit val displayKeywords: Display[Keyword] =
    Display.byShortName(_.label)

  implicit val displayTacCategory: Display[TacCategory] =
    Display.byShortName(_.label)

  implicit val displayToOActivation: Display[ToOActivation] =
    Display.byShortName(_.label)

  implicit val displayMagnitudeBand: Display[MagnitudeBand] =
    Display.by(_.shortName, _.longName)

  implicit val displayMagnitudeSystem: Display[MagnitudeSystem] =
    Display.byShortName(_.tag)
}
