// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats._
import coulomb._
import coulomb.accepted.Percent
import coulomb.time.Hour
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.Interval
import eu.timepit.refined.types.numeric.NonNegDouble
import explore.model.enum._
import lucuma.core.model.Partner
import lucuma.core.model.StandardUser
import monocle.macros.Lenses
import monocle.Optional

@Lenses
final case class ProposalDetails(
  title:          String,
  pi:             StandardUser,
  proposalClass:  ProposalClass,
  category:       Option[TacCategory],
  toOActivation:  ToOActivation,
  keywords:       Set[Keyword],
  abstrakt:       String,
  partnerSplits:  List[PartnerSplit],
  band1And2Hours: ProposalDetails.NonNegHour,
  band3Hours:     ProposalDetails.NonNegHour
)

object ProposalDetails {
  type NonNegHour = Quantity[NonNegDouble, Hour]
  implicit val equalProposalDetails: Eq[ProposalDetails] = Eq.fromUniversalEquals

  val optionalCategory: Optional[ProposalDetails, TacCategory] =
    Optional(category.get)(c => category.set(Some(c)))
}

@Lenses
final case class PartnerSplit(partner: Partner, percent: PartnerSplit.IntPercent)

object PartnerSplit {
  type ZeroTo100  = Interval.Closed[0, 100]
  type IntPercent = Quantity[Int Refined ZeroTo100, Percent]
  implicit val equalPartnerSplit: Eq[PartnerSplit] = Eq.fromUniversalEquals
}
