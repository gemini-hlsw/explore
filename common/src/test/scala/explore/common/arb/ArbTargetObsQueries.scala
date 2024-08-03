// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

// package explore.common.arb

// import cats.syntax.all.*
// import explore.common.TargetObsQueries.*
// import explore.common.TargetObsQueriesGQL
// import explore.model.arb.ArbConstraintsSummary
// import lucuma.core.arb.ArbTime
// import lucuma.core.enums.ObsActiveStatus
// import lucuma.core.enums.ObsStatus
// import lucuma.core.model.Asterism
// import explore.model.Observation
// import lucuma.core.model.Target
// import lucuma.core.util.arb.ArbEnumerated.*
// import lucuma.core.util.arb.ArbGid.*
// import org.scalacheck.Arbitrary
// import org.scalacheck.Arbitrary.*
// import org.scalacheck.Cogen
// import org.scalacheck.Cogen.*
// import org.scalacheck.Gen

// import java.time.Duration

// trait ArbTargetObsQueries {
//   import ArbConstraintsSummary.*
//   import ArbTime.*

//   type ConstraintSet = TargetObsQueriesGQL.TargetsObsQuery.Data.Observations.Nodes.ConstraintSet
//   val ConstraintSet = TargetObsQueriesGQL.TargetsObsQuery.Data.Observations.Nodes.ConstraintSet

//   implicit val arbObsResultPointing =
//     Arbitrary[ObsResult.Pointing] {
//       Gen.oneOf(arbitrary[Target.Id].map(ObsResult.Pointing.Target.apply),
//                 arbitrary[Asterism.Id].map(ObsResult.Pointing.Asterism.apply)
//       )
//     }

//   implicit val cogenObsResultPointing: Cogen[ObsResult.Pointing] =
//     Cogen[Either[Asterism.Id, Target.Id]]
//       .contramap {
//         case ObsResult.Pointing.Target(id)   => id.asRight
//         case ObsResult.Pointing.Asterism(id) => id.asLeft
//       }

//   implicit val arbConstraintSet = buildConstraintsSummaryArb(ConstraintSet.apply)

//   implicit val arbObsResult =
//     Arbitrary[ObsResult] {
//       for {
//         id            <- arbitrary[Observation.Id]
//         pointing      <- arbitrary[Option[ObsResult.Pointing]]
//         constraintSet <- arbitrary[ConstraintSet]
//         status        <- arbitrary[ObsStatus]
//         activeStatus  <- arbitrary[ObsActiveStatus]
//         duration      <- arbitrary[Duration]
//       } yield ObsResult(id,
//                         pointing,
//                         constraintSet,
//                         status,
//                         activeStatus,
//                         ObsResult.PlannedTime(duration)
//       )
//     }
// }

// object ArbTargetObsQueries extends ArbTargetObsQueries
