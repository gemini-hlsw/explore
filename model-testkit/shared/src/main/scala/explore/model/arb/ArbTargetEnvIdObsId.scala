// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import explore.model.TargetEnvIdObsId
import lucuma.core.arb._
import lucuma.core.model.Observation
import lucuma.core.model.TargetEnvironment
import lucuma.core.util.arb.ArbGid._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait ArbTargetEnvIdObsId {
  implicit val arbTargetEnvIdObsId = Arbitrary[TargetEnvIdObsId] {
    for {
      tenvId   <- arbitrary[TargetEnvironment.Id]
      optObsId <- arbitrary[Option[Observation.Id]]
    } yield TargetEnvIdObsId((tenvId, optObsId))
  }

  implicit val cogenTargetEnvIdObsId: Cogen[TargetEnvIdObsId] =
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

  val stringsOftenParsable: Gen[String] = arbitrary[TargetEnvIdObsId]
    .map(TargetEnvIdObsId.format.reverseGet)
    .flatMapOneOf(Gen.const, perturbations: _*)
}

object ArbTargetEnvIdObsId extends ArbTargetEnvIdObsId
