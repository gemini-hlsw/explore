// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.undo

import scala.collection.immutable.ListMap

import cats.implicits._
import cats.kernel.Eq
import explore.data.KeyedIndexedList
import monocle.Getter
import monocle.Iso
import monocle.Lens
import mouse.boolean._

class KIListMod[F[_], A, K: Eq](protected val keyLens: Lens[A, K])
    extends IndexedCollMod[F, KeyedIndexedList, Int, A, cats.Id, K] {

  override protected val valueLens: Lens[A, A] = Iso.id.asLens
  override protected val pureNode              = identity

  override def getterForKey(key: K): Getter[KeyedIndexedList[K, A], Option[(A, Int)]] =
    Getter[KeyedIndexedList[K, A], Option[(A, Int)]](_.getElemAndIndex(key))

  override def removeWithKey(kiList: KeyedIndexedList[K, A], key: K): KeyedIndexedList[K, A] =
    kiList.removed(key)

  override def insertWithIdx(
    kiList: KeyedIndexedList[K, A],
    idx:    Int,
    a:      A
  ): KeyedIndexedList[K, A] = {
    val key = keyLens.get(a)
    kiList.inserted(key, a, idx)
  }
}
