// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import explore.model.PosAngle
import lucuma.core.math.arb.ArbAngle._
import lucuma.core.math.Angle
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait ArbPosAngle {

  implicit val fixedPosAngleArb = Arbitrary[PosAngle.Fixed] {
    for {
      a <- arbitrary[Angle]
    } yield PosAngle.Fixed(a)
  }

  implicit def fixedPosAngleCogen: Cogen[PosAngle.Fixed] =
    Cogen[Angle].contramap(_.angle)

  implicit val allowFlipPosAngleArb = Arbitrary[PosAngle.AllowFlip] {
    for {
      a <- arbitrary[Angle]
    } yield PosAngle.AllowFlip(a)
  }

  implicit def allowFlipCogen: Cogen[PosAngle.AllowFlip] =
    Cogen[Angle].contramap(_.angle)

  implicit val parallacticOverridePosAngleArb = Arbitrary[PosAngle.ParallacticOverride] {
    for {
      a <- arbitrary[Angle]
    } yield PosAngle.ParallacticOverride(a)
  }

  implicit def parallacticOverridePosAngleCogen: Cogen[PosAngle.ParallacticOverride] =
    Cogen[Angle].contramap(_.angle)

  implicit val posAngleArb = Arbitrary[PosAngle] {
    for {
      f <- arbitrary[PosAngle.Fixed]
      v <- arbitrary[PosAngle.AllowFlip]
      p <- Gen.const(PosAngle.AverageParallactic)
      o <- arbitrary[PosAngle.ParallacticOverride]
      u <- Gen.const(PosAngle.Unconstrained)
      a <- Gen.oneOf(f, v, p, o, u)
    } yield a
  }

  implicit def posAngleCogen: Cogen[PosAngle] =
    Cogen[Option[Option[Either[Angle, Either[Angle, Either[Angle, Angle]]]]]].contramap {
      case PosAngle.AverageParallactic     => None
      case PosAngle.Unconstrained          => Some(None)
      case PosAngle.Fixed(a)               => Some(Some(Left(a)))
      case PosAngle.AllowFlip(a)           => Some(Some(Right(Left(a))))
      case PosAngle.ParallacticOverride(a) => Some(Some(Right(Right(Left(a)))))
    }
}

object ArbPosAngle extends ArbPosAngle
