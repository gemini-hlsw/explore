// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.kernel.Eq
import explore.optics.*
import explore.optics.all.*
import lucuma.core.util.arb.ArbEnumerated.*
import lucuma.schemas.model.ObservingMode.GmosSouthLongSlit
import lucuma.schemas.model.arb.ArbObservingMode.given
import monocle.Focus
import monocle.Lens
import monocle.law.discipline.LensTests
import munit.DisciplineSuite
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*

case class Inner[A](a: A)
object Inner {
  def a[A]: Lens[Inner[A], A] = Focus[Inner[A]](_.a)

  implicit def eqInner[A: Eq]: Eq[Inner[A]] = Eq.by(_.a)
}

case class Outer[A](opt: Option[Inner[A]])
object Outer {
  def opt[A]: Lens[Outer[A], Option[Inner[A]]] = Focus[Outer[A]](_.opt)

  implicit def eqOuter[A: Eq]: Eq[Outer[A]] = Eq.by(_.opt)
}

class OpticsSuite extends DisciplineSuite {

  implicit def wrapArb[A: Arbitrary]: Arbitrary[Inner[A]] =
    Arbitrary[Inner[A]] {
      arbitrary[A].map(Inner.apply)
    }

  implicit def outerArb[A: Arbitrary]: Arbitrary[Outer[A]] =
    Arbitrary[Outer[A]] {
      arbitrary[Option[Inner[A]]].map(Outer.apply)
    }

  def adjuster[A]: Adjuster[Outer[A], Option[A]] =
    Outer.opt[A].asAdjuster.composeOptionLens(Inner.a[A])

  val adjusterInt = adjuster[Int]

  checkAll("Adjuster.composeOptionLens", AdjusterTests(adjusterInt))

  val disjointZip2 =
    disjointZip(
      GmosSouthLongSlit.grating,
      GmosSouthLongSlit.filter
    )
  val disjointZip3 =
    disjointZip(
      GmosSouthLongSlit.grating,
      GmosSouthLongSlit.filter,
      GmosSouthLongSlit.fpu
    )
  val disjointZip4 =
    disjointZip(
      GmosSouthLongSlit.grating,
      GmosSouthLongSlit.filter,
      GmosSouthLongSlit.fpu,
      GmosSouthLongSlit.explicitRoi
    )

  checkAll("disjointZip2", LensTests(disjointZip2))
  checkAll("disjointZip3", LensTests(disjointZip3))
  checkAll("disjointZip4", LensTests(disjointZip4))
}
