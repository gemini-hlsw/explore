// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common.arb

import cats.syntax.all._
import lucuma.core.util.arb.ArbEnumerated._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen
import org.scalacheck.Cogen._
import lucuma.core.util.arb.ArbGid._
import org.scalacheck.Gen
import lucuma.core.model.Target
import lucuma.core.model.Asterism
import explore.common.TargetObsQueries._
import lucuma.core.model.Observation
import explore.model.ConstraintsSummary
import explore.model.arb.ArbConstraintsSummary

trait ArbTargetObsQueries {
  import ArbConstraintsSummary._

  implicit val arbObsResultPointing =
    Arbitrary[ObsResult.Pointing] {
      Gen.oneOf(arbitrary[Target.Id].map(ObsResult.Pointing.Target.apply),
                arbitrary[Asterism.Id].map(ObsResult.Pointing.Asterism.apply)
      )
    }

  implicit val cogenbsResultPointing: Cogen[ObsResult.Pointing] =
    Cogen[Either[Asterism.Id, Target.Id]]
      .contramap {
        case ObsResult.Pointing.Target(id)   => id.asRight
        case ObsResult.Pointing.Asterism(id) => id.asLeft
      }

  implicit val arbObsResult =
    Arbitrary[ObsResult] {
      for {
        id            <- arbitrary[Observation.Id]
        pointing      <- arbitrary[Option[ObsResult.Pointing]]
        constraintSet <- arbitrary[Option[ConstraintsSummary]]
      } yield ObsResult(id, pointing, constraintSet)
    }
}

object ArbTargetObsQueries extends ArbTargetObsQueries
