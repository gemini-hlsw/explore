// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.data

import cats.Eq

import scala.collection.immutable.TreeSeqMap

// Each element has a unique Key.
// Efficient loookup of elements and positions by Key.
case class KeyedIndexedList[K, A] private (private val list: TreeSeqMap[K, (A, Int)]) {
  def getElemAndIndex(key: K): Option[(A, Int)] = list.get(key)
  def getElement(key:      K): Option[A]        = list.get(key).map(_._1)
  def getIndex(key:        K): Option[Int]      = list.get(key).map(_._2)

  def elements: Iterable[A] = list.values.map(_._1)

  def toList: List[A] = elements.toList

  def length: Int = list.size

  def removed(key: K): KeyedIndexedList[K, A] =
    getIndex(key)
      .fold(this)(idx =>
        KeyedIndexedList.unsafeFromTreeSeqMap(
          list
            .removed(key)
            .map(_ match {
              case (key, (a, i)) if i < idx => (key, (a, i))
              case (key, (a, i)) if i > idx => (key, (a, i - 1))
            })
        )
      )

  def contains(key: K): Boolean = list.contains(key)

  def exists(p: A => Boolean): Boolean = elements.exists(p)

  def take(n: Int): KeyedIndexedList[K, A] = KeyedIndexedList.unsafeFromTreeSeqMap(list.take(n))
  def drop(n: Int): KeyedIndexedList[K, A] =
    KeyedIndexedList.unsafeFromTreeSeqMap(
      list
        .collect {
          case (id, (a, i)) if i >= n => (id, (a, i - n))
        }
    )

  def inserted(key: K, elem: A, idx: Int): KeyedIndexedList[K, A] = {
    val fixedIdx = idx match {
      case i if i > length => length
      case i if i < 0      => 0
      case i               => i
    }
    val baseList = removed(key).list
    KeyedIndexedList.unsafeFromTreeSeqMap(
      (baseList
        .take(fixedIdx) + ((key, (elem, fixedIdx)))) ++ baseList.drop(fixedIdx).map {
        case (k, (e, i)) => (k, (e, i + 1))
      }
    )
  }

  def updated(key: K, value: A, idx: Int): KeyedIndexedList[K, A] =
    if (contains(key))
      inserted(key, value, idx)
    else
      this
}

object KeyedIndexedList {
  def empty[K, A]: KeyedIndexedList[K, A] = KeyedIndexedList[K, A](TreeSeqMap.empty)

  def fromList[K, A](list: List[A], getKey: A => K): KeyedIndexedList[K, A] =
    KeyedIndexedList(TreeSeqMap.from(list.distinctBy(getKey).zipWithIndex.map { case (a, idx) =>
      (getKey(a), (a, idx))
    }))

  def unsafeFromTreeSeqMap[K, A](list: TreeSeqMap[K, (A, Int)]): KeyedIndexedList[K, A] =
    KeyedIndexedList(list)

  implicit def eqKeyedIndexedList[K: Eq, A: Eq]: Eq[KeyedIndexedList[K, A]] =
    Eq.by(_.list: Map[K, (A, Int)])
}
