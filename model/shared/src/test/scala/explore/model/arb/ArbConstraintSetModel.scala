// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import explore.model.ConstraintSetModel
import lucuma.core.enum._
import lucuma.core.model.ConstraintSet
import lucuma.core.util.arb.ArbEnumerated._
import lucuma.core.util.arb.ArbGid._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen
import org.scalacheck.Cogen._
// these need to be at the end to avoid diverging implicit expansion problems
import eu.timepit.refined.scalacheck.string._
import eu.timepit.refined.types.string._
import explore.model.ElevationRange

trait ArbConstraintSetModel {
  import ArbElevationRange._

  implicit val constraintSetModelArb: Arbitrary[ConstraintSetModel] = Arbitrary {
    for {
      id   <- arbitrary[ConstraintSet.Id]
      name <- arbitrary[NonEmptyString]
      iq   <- arbitrary[ImageQuality]
      ce   <- arbitrary[CloudExtinction]
      sb   <- arbitrary[SkyBackground]
      wv   <- arbitrary[WaterVapor]
      er   <- arbitrary[ElevationRange]
    } yield ConstraintSetModel(id, name, iq, ce, sb, wv, er)
  }

  implicit val constraintSetModelCogen: Cogen[ConstraintSetModel] =
    Cogen[
      (
        ConstraintSet.Id,
        String,
        ImageQuality,
        CloudExtinction,
        SkyBackground,
        WaterVapor,
        ElevationRange
      )
    ]
      .contramap(cs =>
        (cs.id,
         cs.name.value,
         cs.imageQuality,
         cs.cloudExtinction,
         cs.skyBackground,
         cs.waterVapor,
         cs.elevationRange
        )
      )
}

object ArbConstraintSetModel extends ArbConstraintSetModel
