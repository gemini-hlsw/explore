// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.implicits._
import munit.DisciplineSuite
import monocle.macros.Lenses
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import cats.kernel.Eq
import explore.optics._

class OpticsSuite extends DisciplineSuite {

  @Lenses
  case class Inner[A](a: A)
  object Inner {
    implicit def eqInner[A: Eq]: Eq[Inner[A]] = Eq.by(_.a)
  }

  @Lenses
  case class Outer[A](opt: Option[Inner[A]])
  object Outer {
    implicit def eqOuter[A: Eq]: Eq[Outer[A]] = Eq.by(_.opt)
  }

  implicit def wrapArb[A: Arbitrary] =
    Arbitrary[Inner[A]] {
      arbitrary[A].map(Inner.apply)
    }

  implicit def outerArb[A: Arbitrary] =
    Arbitrary[Outer[A]] {
      arbitrary[Option[Inner[A]]].map(Outer.apply)
    }

  def adjuster[A]: Adjuster[Outer[A], Option[A]] =
    Outer.opt[A].asAdjuster.composeOptionLens(Inner.a[A])

  val adjusterInt = adjuster[Int]

  checkAll("Adjuster.composeOptionLens", AdjusterTests(adjusterInt))
}
