// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import cats.syntax.all._
import lucuma.core.util.arb.ArbEnumerated._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen
import org.scalacheck.Cogen._
import explore.model.Pointing
import lucuma.core.util.arb.ArbGid._
import org.scalacheck.Gen
import lucuma.core.model.Target
import lucuma.core.model.Asterism
import eu.timepit.refined.scalacheck._
import eu.timepit.refined.scalacheck.string._
import eu.timepit.refined.types.string._

trait ArbPointing {
  implicit val arbPointingTarget =
    Arbitrary[Pointing.PointingTarget] {
      for {
        id   <- arbitrary[Target.Id]
        name <- arbitrary[NonEmptyString]
      } yield Pointing.PointingTarget(id, name)
    }

  implicit val arbPointingAsterism =
    Arbitrary[Pointing.PointingAsterism] {
      for {
        id      <- arbitrary[Asterism.Id]
        name    <- arbitrary[Option[NonEmptyString]]
        targets <- arbitrary[List[Pointing.PointingTarget]]
      } yield Pointing.PointingAsterism(id, name, targets)
    }

  implicit val arbPointing =
    Arbitrary[Pointing] {
      Gen.oneOf(arbitrary[Pointing.PointingTarget], arbitrary[Pointing.PointingAsterism])
    }

  implicit val cogenPointingTarget: Cogen[Pointing.PointingTarget] =
    Cogen[(Target.Id, NonEmptyString)].contramap(t => (t.id, t.name))

  implicit val cogenPointingAsterism: Cogen[Pointing.PointingAsterism] =
    Cogen[(Asterism.Id, Option[NonEmptyString], List[Pointing.PointingTarget])].contramap(a =>
      (a.id, a.name, a.targets)
    )

  implicit val cogenPointing: Cogen[Pointing] =
    Cogen[Either[Pointing.PointingAsterism, Pointing.PointingTarget]]
      .contramap {
        case t: Pointing.PointingTarget   => t.asRight
        case a: Pointing.PointingAsterism => a.asLeft
      }
}

object ArbPointing extends ArbPointing
