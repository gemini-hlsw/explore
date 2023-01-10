// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import lucuma.core.model.Observation

sealed trait LocalClipboard

object LocalClipboard:
  case object Empty                             extends LocalClipboard
  case class CopiedObservations(oids: ObsIdSet) extends LocalClipboard
  case class CopiedTargets(tids: TargetIdSet)   extends LocalClipboard
