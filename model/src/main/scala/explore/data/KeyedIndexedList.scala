// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.data

import scala.collection.immutable.ListMap

import cats.implicits._
import cats.kernel.Eq
import monocle.Lens

// Each element has a unique Key.
// Elements can be accessed by Key in O(1).
// Position in the list (index) can be accessed by Key in O(1).
case class KeyedIndexedList[K, A] private (private val list: ListMap[K, (A, Int)]) {
  def getElemAndIndex(id: K): Option[(A, Int)] = list.get(id)
  def getElement(id:      K): Option[A]        = list.get(id).map(_._1)
  def getIndex(id:        K): Option[Int]      = list.get(id).map(_._2)

  def elements: Iterable[A] = list.values.map(_._1)

  def toList: List[A] = elements.toList

  def length: Int = list.size

  def removed(key: K): KeyedIndexedList[K, A] =
    getIndex(key)
      .fold(this)(idx =>
        KeyedIndexedList.unsafeFromListMap(
          list
            .removed(key)
            .map(_ match {
              case (id, (a, i)) if i < idx => (id, (a, i))
              case (id, (a, i)) if i > idx => (id, (a, i - 1))
            })
        )
      )

  def contains(key: K): Boolean = list.contains(key)

  def exists(p: A => Boolean): Boolean = elements.exists(p)

  def take(n: Int): KeyedIndexedList[K, A] = KeyedIndexedList.unsafeFromListMap(list.take(n))
  def drop(n: Int): KeyedIndexedList[K, A] = KeyedIndexedList.unsafeFromListMap(list.drop(n))

  def inserted(key: K, elem: A, idx: Int): KeyedIndexedList[K, A] = {
    val fixedIdx = idx match {
      case i if i > length => length
      case i if i < 0      => 0
      case i               => i
    }
    val baseList = removed(key).list
    KeyedIndexedList.unsafeFromListMap(
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
  def fromList[K, A](list: List[A], keyGet: A => K): KeyedIndexedList[K, A] =
    KeyedIndexedList(ListMap.from(list.distinctBy(keyGet).zipWithIndex.map {
      case (a, idx) => (keyGet(a), (a, idx))
    }))

  def unsafeFromListMap[K, A](list: ListMap[K, (A, Int)]): KeyedIndexedList[K, A] =
    KeyedIndexedList(list)

  implicit def eqIdList[K: Eq, A: Eq]: Eq[KeyedIndexedList[K, A]] = Eq.by(_.list: Map[K, (A, Int)])
}
