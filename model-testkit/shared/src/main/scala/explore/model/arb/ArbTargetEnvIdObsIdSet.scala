// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import cats.data.NonEmptySet
import cats.laws.discipline.arbitrary._
import explore.model.TargetEnvIdObsId
import explore.model.TargetEnvIdObsIdSet
import lucuma.core.arb._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait ArbTargetEnvIdObsIdSet {
  import ArbTargetEnvIdObsId._

  implicit val arbTargetEnvIdObsIdSet: Arbitrary[TargetEnvIdObsIdSet] = Arbitrary {
    arbitrary[NonEmptySet[TargetEnvIdObsId]].map(TargetEnvIdObsIdSet.apply)
  }

  implicit val cogenTargetEnvIdObsIdSet: Cogen[TargetEnvIdObsIdSet] =
    Cogen[NonEmptySet[TargetEnvIdObsId]].contramap(_.idSet)

  private val perturbations: List[String => Gen[String]] =
    List(
      _ => arbitrary[String],                        // swap for a random string
      s => Gen.const(s.replaceFirst("t", "x")),      // make a target env id invalid
      s => Gen.const(s.replaceFirst("o", "x")),      // make an obs id invalid
      s => Gen.const(s.replaceFirst("none", "fun")), // change a None indicator
      s => Gen.const(s.replace(",", ""))             // change separator
    )

  val stringsOftenParsable: Gen[String] = arbitrary[TargetEnvIdObsIdSet]
    .map(TargetEnvIdObsIdSet.format.reverseGet)
    .flatMapOneOf(Gen.const, perturbations: _*)
}

object ArbTargetEnvIdObsIdSet extends ArbTargetEnvIdObsIdSet
