// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.undo

import eu.timepit.refined.scalacheck.numeric.*
import explore.data.KeyedIndexedList
import explore.optics.AdjusterTests
import monocle.Focus
import monocle.Iso
import monocle.Lens
import munit.DisciplineSuite
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*

class ListModSpec extends DisciplineSuite {

  val idGet = Iso.id[Int].get

  val listMod = new KIListMod[Int, Int](Iso.id)

  val elemWithKey = listMod.withKey(0)
  val posWithKey  = listMod.pos.withKey(0)

  implicit def kiListArb[Key, A: Arbitrary](implicit
    keyGet: A => Key
  ): Arbitrary[KeyedIndexedList[Key, A]] =
    Arbitrary[KeyedIndexedList[Key, A]] {
      arbitrary[List[A]].map(list => KeyedIndexedList.fromList(list, keyGet))
    }

  checkAll("listMod.withKey.adjuster", AdjusterTests(elemWithKey.adjuster))
  checkAll("listMod.pos.withKey.adjuster", AdjusterTests(posWithKey.adjuster))

  case class V(id: Int, s: String)
  object V {
    val id: Lens[V, Int] = Focus[V](_.id)

    def apply(id: Int): V = V(id, id.toString)
  }

  implicit val keyGet: V => Int = V.id.get

  val vlistMod = new KIListMod[V, Int](V.id)

  val vWithId    = listMod.withKey(0)
  val vPosWithId = listMod.pos.withKey(0)

  checkAll("listMod.withKey.adjuster", AdjusterTests(vWithId.adjuster))
  checkAll("listMod.pos.withKey.adjuster", AdjusterTests(vPosWithId.adjuster))
}
