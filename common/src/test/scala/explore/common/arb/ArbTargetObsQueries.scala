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
import explore.model.arb.ArbConstraintsSummary
import explore.common.TargetObsQueriesGQL
import lucuma.core.enum.ObsStatus
import java.time.Duration
import lucuma.core.arb.ArbTime

trait ArbTargetObsQueries {
  import ArbConstraintsSummary._
  import ArbTime._

  type ConstraintSet = TargetObsQueriesGQL.TargetsObsQuery.Data.Observations.Nodes.ConstraintSet
  val ConstraintSet = TargetObsQueriesGQL.TargetsObsQuery.Data.Observations.Nodes.ConstraintSet

  implicit val arbObsResultPointing =
    Arbitrary[ObsResult.Pointing] {
      Gen.oneOf(arbitrary[Target.Id].map(ObsResult.Pointing.Target.apply),
                arbitrary[Asterism.Id].map(ObsResult.Pointing.Asterism.apply)
      )
    }

  implicit val cogenObsResultPointing: Cogen[ObsResult.Pointing] =
    Cogen[Either[Asterism.Id, Target.Id]]
      .contramap {
        case ObsResult.Pointing.Target(id)   => id.asRight
        case ObsResult.Pointing.Asterism(id) => id.asLeft
      }

  implicit val arbConstraintSet = buildConstraintsSummaryArb(ConstraintSet.apply)

  implicit val arbObsResult =
    Arbitrary[ObsResult] {
      for {
        id            <- arbitrary[Observation.Id]
        pointing      <- arbitrary[Option[ObsResult.Pointing]]
        constraintSet <- arbitrary[ConstraintSet]
        status        <- arbitrary[ObsStatus]
        duration      <- arbitrary[Duration]
      } yield ObsResult(id, pointing, constraintSet, status, ObsResult.PlannedTime(duration))
    }
}

object ArbTargetObsQueries extends ArbTargetObsQueries
