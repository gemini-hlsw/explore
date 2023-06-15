// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.enums

import lucuma.core.util.Enumerated

enum GridLayoutSection(val value: String) derives Enumerated:
  case ObservationsLayout extends GridLayoutSection("observations")
  case TargetLayout       extends GridLayoutSection("targets")
  case ConstraintsLayout  extends GridLayoutSection("constraints")
  case SchedulingLayout   extends GridLayoutSection("scheduling")
  case OverviewLayout     extends GridLayoutSection("overview")

  private val tag = value
