// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.enums

import eu.timepit.refined.auto.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.refined.*

enum PosAngleOptions(val tag: NonEmptyString, val longName: NonEmptyString):
  case Fixed extends PosAngleOptions("Fixed".refined, "Fixed".refined)

  case AllowFlip extends PosAngleOptions("AllowFlip".refined, "Allow 180° flip".refined)

  case AverageParallactic
      extends PosAngleOptions("AverageParallactic".refined, "Average Parallactic".refined)

  case ParallacticOverride
      extends PosAngleOptions("ParallacticOverride".refined, "Parallactic Override".refined)

  case Unconstrained extends PosAngleOptions("Unconstrained".refined, "Unconstrained".refined)

object PosAngleOptions:
  given Enumerated[PosAngleOptions] =
    Enumerated
      .from(Fixed, AllowFlip, AverageParallactic, ParallacticOverride, Unconstrained)
      .withTag(_.tag)

  given Display[PosAngleOptions] =
    Display.by(_.longName.value, _.longName.value)
