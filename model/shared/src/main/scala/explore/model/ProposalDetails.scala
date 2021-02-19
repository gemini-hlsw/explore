// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats._
import explore.model.enum._
import explore.model.refined._
import lucuma.core.model.Partner
import lucuma.core.model.StandardUser
import monocle.macros.Lenses

@Lenses
final case class ProposalDetails(
  title:         String,
  pi:            StandardUser,
  proposalClass: ProposalClass,
  category:      Option[TacCategory],
  toOActivation: ToOActivation,
  keywords:      Set[Keyword],
  abstrakt:      String,
  partnerSplits: List[PartnerSplit],
  // The 2 times represent "Band 1&2" and "Band 3" for queue
  // proposals and "First Semester" and "Total" for Large
  // proposals and Subaru Intensive proposals. Other proposal
  // classes only have one time. For now I am reusing the fields
  // as needed. This could shake out differently in the final model
  // depending things such as how the times are calculated from
  // the observations.
  requestTime1:  NonNegHour,
  requestTime2:  NonNegHour,
  minimumPct1:   IntPercent,
  minimumPct2:   IntPercent)

object ProposalDetails {
  implicit val equalProposalDetails: Eq[ProposalDetails] = Eq.fromUniversalEquals
}

@Lenses
final case class PartnerSplit(partner: Partner, percent: IntPercent)

object PartnerSplit {
  implicit val equalPartnerSplit: Eq[PartnerSplit] = Eq.fromUniversalEquals
}
