// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.enums

import lucuma.core.util.Enumerated

enum ScienceSubtype(val tag: String) derives Enumerated:
  case Classical          extends ScienceSubtype("classical")
  case DirectorsTime      extends ScienceSubtype("DirectorsTime")
  case FastTurnaround     extends ScienceSubtype("FastTurnaround")
  case LargeProgram       extends ScienceSubtype("LargeProgram")
  case PoorWeather        extends ScienceSubtype("PoorWeather")
  case Queue              extends ScienceSubtype("Queue")
  case DemoScience        extends ScienceSubtype("DemoScience")
  case SystemVerification extends ScienceSubtype("SystemVerification")
