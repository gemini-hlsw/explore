// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

enum LocalClipboard(val isEmpty: Boolean, val isObservations: Boolean, val isTargets: Boolean):
  case Empty                              extends LocalClipboard(true, false, false)
  case CopiedObservations(oids: ObsIdSet) extends LocalClipboard(false, true, false)
  case CopiedTargets(tids: TargetIdSet)   extends LocalClipboard(false, false, true)
