// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.undo

import explore.data.KeyedIndexedList
import monocle.Getter
import monocle.Iso
import monocle.Lens

case class KIListMod[A, K](protected val keyLens: Lens[A, K])
    extends IndexedCollMod[KeyedIndexedList, Int, A, cats.Id, K] {

  override protected val valueLens: Lens[A, A] = Iso.id.asLens
  override protected val pureNode              = identity

  override def getterForKey(key: K): Getter[KeyedIndexedList[K, A], Option[(A, Int)]]        =
    Getter[KeyedIndexedList[K, A], Option[(A, Int)]](_.getElemAndIndex(key))

  override def removeWithKey(kiList: KeyedIndexedList[K, A], key: K): KeyedIndexedList[K, A] =
    kiList.removed(key)

  override def insertWithIdx(
    kiList: KeyedIndexedList[K, A],
    idx:    Int,
    elem:   A
  ): KeyedIndexedList[K, A] = {
    val key = keyLens.get(elem)
    kiList.inserted(key, elem, idx)
  }
}
