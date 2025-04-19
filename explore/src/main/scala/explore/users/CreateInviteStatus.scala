// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.users

import lucuma.core.util.Enumerated

enum CreateInviteStatus(private val tag: String) derives Enumerated:
  case Idle    extends CreateInviteStatus("idle")
  case Running extends CreateInviteStatus("running")
  case Error   extends CreateInviteStatus("error")
  case Done    extends CreateInviteStatus("done")
