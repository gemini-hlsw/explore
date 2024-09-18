// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.users

import lucuma.core.util.Enumerated

enum CreateInviteProcess(private val tag: String) derives Enumerated:
  case Idle    extends CreateInviteProcess("idle")
  case Running extends CreateInviteProcess("running")
  case Error   extends CreateInviteProcess("error")
  case Done    extends CreateInviteProcess("done")
