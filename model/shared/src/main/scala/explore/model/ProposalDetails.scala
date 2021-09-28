// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats._
import explore.model.enum._
import explore.model.refined._
import lucuma.core.model.Partner
import lucuma.core.model.StandardUser
import monocle.Focus

final case class ProposalDetails(
  title:         String,
  pi:            StandardUser,
  proposalClass: ProposalClass,
  category:      Option[TacCategory],
  toOActivation: ToOActivation,
  abstrakt:      String,
  partnerSplits: List[PartnerSplit],
  // The 2 times represent "Band 1&2" and "Band 3" for queue
  // proposals and "First Semester" and "Total" for Large
  // proposals and Subaru Intensive proposals. Other proposal
  // classes only have one time. For now I am reusing the fields
  // as needed. This could shake out differently in the final model
  // depending things such as how the times are calculated from
  // the observations.
  // TODO: Perhaps use something other than a Double in NonNegHour. Reusability[ProposalDetails] could then be derived and Eq instance changed.
  requestTime1: NonNegHour,
  requestTime2: NonNegHour,
  minimumPct1:  IntPercent,
  minimumPct2:  IntPercent
)

object ProposalDetails {
  val title         = Focus[ProposalDetails](_.title)
  val pi            = Focus[ProposalDetails](_.pi)
  val proposalClass = Focus[ProposalDetails](_.proposalClass)
  val toOActivation = Focus[ProposalDetails](_.toOActivation)
  val partnerSplits = Focus[ProposalDetails](_.partnerSplits)
  val category      = Focus[ProposalDetails](_.category)
  val requestTime1  = Focus[ProposalDetails](_.requestTime1)
  val requestTime2  = Focus[ProposalDetails](_.requestTime1)
  val minimumPct1   = Focus[ProposalDetails](_.minimumPct1)
  val minimumPct2   = Focus[ProposalDetails](_.minimumPct2)
  val abstrakt      = Focus[ProposalDetails](_.abstrakt)

  implicit val equalProposalDetails: Eq[ProposalDetails] = Eq.fromUniversalEquals
}

final case class PartnerSplit(partner: Partner, percent: IntPercent)

object PartnerSplit {
  val percent = Focus[PartnerSplit](_.percent)

  implicit val equalPartnerSplit: Eq[PartnerSplit] = Eq.fromUniversalEquals
}
