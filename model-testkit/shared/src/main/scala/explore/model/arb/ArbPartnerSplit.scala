// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import cats.implicits.*
import eu.timepit.refined.scalacheck.all.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enums.TacCategory
import lucuma.core.enums.ToOActivation
import lucuma.core.model.IntPercent
import lucuma.core.model.Partner
import lucuma.core.model.Proposal
import lucuma.core.model.ProposalClass
import lucuma.core.util.arb.ArbCollection
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen.*

import scala.collection.immutable.SortedMap
import lucuma.core.model.ProposalReference
import explore.model.CallForProposal
import explore.model.arb.ArbCallForProposal.given
import explore.model.ProposalType
import explore.model.PartnerSplit

trait ArbPartnerSplit:
  import ArbCollection.given
  import ArbEnumerated.given

  given Arbitrary[PartnerSplit] =
    Arbitrary {
      for {
        partner <- arbitrary[Partner]
        percent <- arbitrary[IntPercent]
      } yield PartnerSplit(partner, percent)
    }

  given Cogen[PartnerSplit] =
    Cogen[
      (Partner, IntPercent)
    ].contramap(p => (p.partner, p.percent))

object ArbPartnerSplit extends ArbPartnerSplit
