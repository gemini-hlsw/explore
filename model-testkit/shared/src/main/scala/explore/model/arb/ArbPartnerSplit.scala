// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import eu.timepit.refined.scalacheck.all.*
import lucuma.core.model.IntPercent
import lucuma.core.model.Partner
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen.*

import explore.model.PartnerSplit

trait ArbPartnerSplit:
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
