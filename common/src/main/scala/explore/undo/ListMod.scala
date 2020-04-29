// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.undo

import cats.implicits._

class ListMod[F[_], A, Id](hasId: Id => A => Boolean) extends IndexedColMod[F, List, Int, A, Id] {

  override def getById(list: List[A], id: Id): Option[(A, Int)] =
    list.indexWhere(hasId(id)).some.filter(_ >= 0).map(i => (list(i), i))

  override def removeWithIdx(list: List[A], idx: Int): List[A] =
    list.take(idx) ++ list.drop(idx + 1)

  override def insertWithIdx(list: List[A], idx: Int, a: A): List[A] =
    list.take(idx) ++ (a +: list.drop(idx))
}
