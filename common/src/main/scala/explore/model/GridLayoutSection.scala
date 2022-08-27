// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import lucuma.core.util.Enumerated

enum GridLayoutSection(val value: String):
  case ObservationsLayout extends GridLayoutSection("observations")
  case TargetLayout       extends GridLayoutSection("targets")

object GridLayoutSection:
  given Enumerated[GridLayoutSection] =
    Enumerated.from(ObservationsLayout, TargetLayout).withTag(_.value)
