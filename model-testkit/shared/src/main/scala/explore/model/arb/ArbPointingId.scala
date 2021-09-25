// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import cats.syntax.all._
import lucuma.core.util.arb.ArbEnumerated._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen
import org.scalacheck.Cogen._
import explore.model.PointingId
import lucuma.core.util.arb.ArbGid._
import org.scalacheck.Gen
import lucuma.core.model.Target
import lucuma.core.model.Asterism

trait ArbPointingId {

  implicit val arbPointingId                      =
    Arbitrary[PointingId] {
      Gen.oneOf(arbitrary[Target.Id].map(PointingId.TargetId.apply),
                arbitrary[Asterism.Id].map(PointingId.AsterismId.apply)
      )
    }

  implicit val cogenPointingId: Cogen[PointingId] =
    Cogen[Either[Asterism.Id, Target.Id]]
      .contramap {
        case PointingId.TargetId(id)   => id.asRight
        case PointingId.AsterismId(id) => id.asLeft
      }
}

object ArbPointingId extends ArbPointingId
