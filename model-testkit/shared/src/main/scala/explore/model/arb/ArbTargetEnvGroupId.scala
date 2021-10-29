// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import explore.model.TargetEnvGroupId
import lucuma.core.arb._
import lucuma.core.model.Observation
import lucuma.core.model.TargetEnvironment
import lucuma.core.util.arb.ArbGid._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait ArbTargetEnvGroupId {
  implicit val arbTargetEnvGroupId = Arbitrary[TargetEnvGroupId] {
    for {
      tenvId   <- arbitrary[TargetEnvironment.Id]
      optObsId <- arbitrary[Option[Observation.Id]]
    } yield TargetEnvGroupId((tenvId, optObsId))
  }

  implicit val cogenTargetEnvGroupId: Cogen[TargetEnvGroupId] =
    Cogen[(TargetEnvironment.Id, Option[Observation.Id])].contramap(t =>
      (t.targetEnvId, t.optObsId)
    )

  private val perturbations: List[String => Gen[String]] =
    List(
      _ => arbitrary[String],                   // swap for a random string
      s => Gen.const(s.replace("t", "x")),      // make target env id invalid
      s => Gen.const(s.replace("o", "x")),      // make obs id invalid
      s => Gen.const(s.replace("none", "fun")), // change the None indicator
      s => Gen.const(s.replace(":", " "))       // change separator
    )

  val stringsOftenParsable: Gen[String] = arbitrary[TargetEnvGroupId]
    .map(TargetEnvGroupId.format.reverseGet)
    .flatMapOneOf(Gen.const, perturbations: _*)
}

object ArbTargetEnvGroupId extends ArbTargetEnvGroupId
