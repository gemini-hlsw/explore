// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.undo

import cats.effect.IO
import cats.implicits._
import explore.data.KeyedIndexedList
import explore.optics.AdjusterTests
import monocle.Getter
import monocle.Lens
import monocle.macros.Lenses
import munit.DisciplineSuite
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._

class ListModSpec extends DisciplineSuite {

  implicit val idGet = Getter.id[Int].get _

  val listMod = new KIListMod[IO, Int, Int](Lens.id)

  val elemWithKey = listMod.withKey(0)
  val posWithKey  = listMod.pos.withKey(0)

  implicit def kiListArb[Key, A: Arbitrary](implicit keyGet: A => Key) =
    Arbitrary[KeyedIndexedList[Key, A]] {
      arbitrary[List[A]].map(list => KeyedIndexedList.fromList(list, keyGet))
    }

  checkAll("listMod.withKey.adjuster", AdjusterTests(elemWithKey.adjuster))
  checkAll("listMod.pos.withKey.adjuster", AdjusterTests(posWithKey.adjuster))

  @Lenses
  case class V(id: Int, s: String)
  object V {
    def apply(id: Int): V = V(id, id.toString)
  }

  implicit val keyGet = V.id.get _

  val vlistMod = new KIListMod[IO, V, Int](V.id)

  val vWithId    = listMod.withKey(0)
  val vPosWithId = listMod.pos.withKey(0)

  checkAll("listMod.withKey.adjuster", AdjusterTests(vWithId.adjuster))
  checkAll("listMod.pos.withKey.adjuster", AdjusterTests(vPosWithId.adjuster))
}
