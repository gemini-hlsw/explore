// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.derived.*
import cats.kernel.Eq
import explore.optics.*
import explore.optics.all.*
import monocle.Focus
import monocle.Lens
import munit.DisciplineSuite
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*

case class Inner[A](a: A, oa: Option[A]) derives Eq
object Inner {
  def a[A]: Lens[Inner[A], A]          = Focus[Inner[A]](_.a)
  def oa[A]: Lens[Inner[A], Option[A]] = Focus[Inner[A]](_.oa)
}

case class Outer[A](opt: Option[Inner[A]])
object Outer {
  def opt[A]: Lens[Outer[A], Option[Inner[A]]] = Focus[Outer[A]](_.opt)

  implicit def eqOuter[A: Eq]: Eq[Outer[A]] = Eq.by(_.opt)
}

class OpticsSuite extends DisciplineSuite {

  implicit def wrapArb[A: Arbitrary]: Arbitrary[Inner[A]] =
    Arbitrary[Inner[A]] {
      for {
        a  <- arbitrary[A]
        oa <- arbitrary[Option[A]]
      } yield Inner(a, oa)
    }

  implicit def outerArb[A: Arbitrary]: Arbitrary[Outer[A]] =
    Arbitrary[Outer[A]] {
      arbitrary[Option[Inner[A]]].map(Outer.apply)
    }

  def adjuster[A]: Adjuster[Outer[A], Option[A]] =
    Outer.opt[A].asAdjuster.composeOptionLens(Inner.a[A])

  val adjusterInt = adjuster[Int]

  def adjusterOption[A]: Adjuster[Outer[A], Option[A]] =
    Outer.opt[A].asAdjuster.composeOptionOptionLens(Inner.oa[A])

  val adjusterOptionInt = adjusterOption[Int]

  checkAll("Adjuster.composeOptionLens", AdjusterTests(adjusterInt))
  checkAll("Adjuster.composeOptionOptionLens", AdjusterTests(adjusterInt))
}
