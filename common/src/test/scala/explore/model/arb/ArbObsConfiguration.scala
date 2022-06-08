// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import explore.model.ObsConfiguration
import lucuma.core.model.PosAngle
import lucuma.core.model.arb.ArbPosAngle._
import lucuma.core.util.arb.ArbEnumerated._
import lucuma.core.util.arb.ArbGid._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen

trait ArbObsConfiguration {

  implicit val obsConfigurationArb = Arbitrary[ObsConfiguration] {
    for {
      pa <- arbitrary[PosAngle]
    } yield ObsConfiguration(pa)
  }

  implicit def obsConfigurationCogen: Cogen[ObsConfiguration] =
    Cogen[PosAngle].contramap(_.posAngle)
}

object ArbObsConfiguration extends ArbObsConfiguration
