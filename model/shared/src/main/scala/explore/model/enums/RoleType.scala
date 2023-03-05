// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.enums

import lucuma.core.util.Enumerated

// Move to lucuma-core
enum RoleType(val tag: String) derives Enumerated:

  case Pi    extends RoleType("PI")
  case NGO   extends RoleType("NGO")
  case Staff extends RoleType("STAFF")
  case Admin extends RoleType("ADMIN")
