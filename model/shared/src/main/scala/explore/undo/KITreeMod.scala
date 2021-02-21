// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.undo

import explore.data.tree._
import monocle.Getter
import monocle.Lens

import KeyedIndexedTree.Index

class KITreeMod[F[_], A, K](protected val keyLens: Lens[A, K])
    extends IndexedCollMod[F, KeyedIndexedTree, Index[K], A, Node, K] {

  override protected val valueLens: Lens[Node[A], A] = Node.value
  override protected val pureNode                    = Node.apply(_)

  override protected def getterForKey(
    key: K
  ): Getter[KeyedIndexedTree[K, A], Option[(Node[A], Index[K])]] =
    Getter[KeyedIndexedTree[K, A], Option[(Node[A], Index[K])]](
      _.getNodeAndIndexByKey(key)
    )

  override def removeWithKey(kiTree: KeyedIndexedTree[K, A], key: K): KeyedIndexedTree[K, A] =
    kiTree.removed(key)

  override def insertWithIdx(
    kiTree: KeyedIndexedTree[K, A],
    idx:    Index[K],
    elem:   Node[A]
  ): KeyedIndexedTree[K, A] = {
    val key = valueLens.composeLens(keyLens).get(elem)
    kiTree.inserted(key, elem, idx)
  }
}
