// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.enums

import lucuma.core.util.Enumerated

enum ProgramUserRole(val tag: String) derives Enumerated {
  case Pi       extends ProgramUserRole("Pi")
  case Coi      extends ProgramUserRole("Coi")
  case Observer extends ProgramUserRole("Observer")
  case Support  extends ProgramUserRole("Support")
}
