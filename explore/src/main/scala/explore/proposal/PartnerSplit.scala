// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.proposal

import lucuma.core.model.IntPercent
import lucuma.core.model.Partner
import monocle.Focus
import monocle.Lens

final case class PartnerSplit(partner: Partner, percent: IntPercent) {
  def toTuple = (partner, percent)
}

object PartnerSplit {
  val partner: Lens[PartnerSplit, Partner]    = Focus[PartnerSplit](_.partner)
  val percent: Lens[PartnerSplit, IntPercent] = Focus[PartnerSplit](_.percent)
}
