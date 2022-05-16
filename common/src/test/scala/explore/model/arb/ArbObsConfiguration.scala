// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import explore.model.ObsConfiguration
import explore.model.PosAngle
import explore.model.arb.ArbPosAngle._
import lucuma.core.arb.ArbTime._
import lucuma.core.util.arb.ArbEnumerated._
import lucuma.core.util.arb.ArbGid._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Cogen._

import java.time.Instant

trait ArbObsConfiguration {

  implicit val obsConfigurationArb = Arbitrary[ObsConfiguration] {
    for {
      pa <- arbitrary[PosAngle]
      ot <- arbitrary[Instant]
    } yield ObsConfiguration(pa, ot)
  }

  implicit def obsConfigurationCogen: Cogen[ObsConfiguration] =
    Cogen[(PosAngle, Instant)].contramap(x => (x.posAngle, x.obsInstant))
}

object ArbObsConfiguration extends ArbObsConfiguration
