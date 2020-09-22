// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats._
import explore.model.enum._
import lucuma.core.model.StandardUser
import monocle.macros.Lenses

@Lenses
final case class ProposalDetails(
  title:         String,
  pi:            StandardUser,
  proposalClass: ProposalClass,
  category:      TacCategory,
  toOActivation: ToOActivation,
  keywords:      Set[Keyword],
  abstrakt:      String
)

object ProposalDetails {
  implicit val equalProposalDetails: Eq[ProposalDetails] = Eq.fromUniversalEquals
}
