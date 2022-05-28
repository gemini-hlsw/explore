// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import cats.data.NonEmptyList
import cats.laws.discipline.arbitrary._
import explore.model.Asterism
import explore.model.TargetWithId
import lucuma.core.model.Target
import lucuma.core.model.arb.ArbTarget._
import lucuma.core.util.arb.ArbGid._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen

trait ArbAsterism {

  implicit val arbTargetWithId: Arbitrary[TargetWithId] = Arbitrary {
    for {
      id <- arbitrary[Target.Id]
      t  <- arbitrary[Target]
    } yield TargetWithId(id, t)
  }

  implicit val cogenTargetWithId: Cogen[TargetWithId] =
    Cogen[(Target.Id, Target)].contramap(x => (x.id, x.target))

  implicit val arbAsterism: Arbitrary[Asterism] = Arbitrary {
    arbitrary[NonEmptyList[TargetWithId]].map(Asterism.apply)
  }

  implicit val cogenAsterism: Cogen[Asterism] =
    Cogen[List[TargetWithId]].contramap(_.asList)
}

object ArbAsterism extends ArbAsterism
