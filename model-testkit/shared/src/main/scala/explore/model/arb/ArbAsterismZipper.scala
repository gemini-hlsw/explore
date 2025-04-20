// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import cats.data.NonEmptyList
import cats.laws.discipline.arbitrary.*
import explore.model.Asterism
import lucuma.core.data.Zipper
import lucuma.schemas.model.*
import lucuma.schemas.model.arb.ArbTargetWithId.given
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen

trait ArbAsterism:
  given arbAsterism: Arbitrary[Asterism] = Arbitrary {
    arbitrary[NonEmptyList[TargetWithId]].map(n => Asterism(Zipper.fromNel(n)))
  }

  given Cogen[Asterism] =
    Cogen[List[TargetWithId]].contramap(_.asList)

object ArbAsterism extends ArbAsterism
