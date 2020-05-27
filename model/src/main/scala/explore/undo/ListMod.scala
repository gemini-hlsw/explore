// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.undo

import cats.implicits._
import cats.kernel.Eq
import monocle.Getter
import monocle.Lens
import monocle.Iso

class ListMod[F[_], A, Id: Eq](protected val idLens: Lens[A, Id])
    extends IndexedCollMod[F, List, Int, A, cats.Id, Id] {

  override protected val valueLens: Lens[A, A] = Iso.id.asLens
  override protected val pureNode              = identity

  override def getterForId(id: Id): Getter[List[A], Option[(A, Int)]] =
    Getter[List[A], Option[(A, Int)]](list =>
      list.indexWhere(a => Eq[Id].eqv(id, idLens.get(a))).some.filter(_ >= 0).map(i => (list(i), i))
    )

  override def removeWithIdx(list: List[A], idx: Int): List[A] =
    list.take(idx) ++ list.drop(idx + 1)

  override def insertWithIdx(list: List[A], idx: Int, a: A): List[A] =
    list.take(idx) ++ (a +: list.drop(idx))
}
