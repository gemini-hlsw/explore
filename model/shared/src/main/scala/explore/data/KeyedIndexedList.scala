// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.data

import cats.Eq

import scala.collection.immutable.TreeSeqMap
import monocle.function.FilterIndex
import monocle.Traversal
import cats.Applicative
import cats.syntax.all.given
import monocle.Focus
import monocle.Lens
import monocle.function.Index
import monocle.function.At
import monocle.function.Index.fromAt

// Each element has a unique Key.
// Efficient loookup of elements and positions by Key.
case class KeyedIndexedList[K, A] private (private val list: TreeSeqMap[K, (A, Int)]):
  def getValueAndIndex(key: K): Option[(A, Int)] = list.get(key)
  def getValue(key:         K): Option[A]        = list.get(key).map(_._1)
  def getIndex(key:         K): Option[Int]      = list.get(key).map(_._2)

  def values: Iterable[A] = list.values.map(_._1)

  def toList: List[A] = values.toList

  def toMap: Map[K, A] = list.map { case (k, (v, _)) => k -> v }

  def collect[B](pf: PartialFunction[(K, (A, Int)), B]): List[B] =
    list.collect(pf).toList

  def length: Int = list.size

  def empty: Boolean = length == 0

  def nonEmpty: Boolean = !empty

  def removed(key: K): KeyedIndexedList[K, A] =
    getIndex(key)
      .fold(this)(idx =>
        KeyedIndexedList.unsafeFromTreeSeqMap(
          list
            .removed(key)
            .map(_ match {
              case (key, (a, i)) if i < idx     => (key, (a, i))
              case (key, (a, i)) /*if i > idx*/ => (key, (a, i - 1))
            })
        )
      )

  def contains(key: K): Boolean = list.contains(key)

  def exists(p: A => Boolean): Boolean = values.exists(p)

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

  def updatedWith(key: K, f: (A, Int) => (A, Int)): KeyedIndexedList[K, A] =
    getValueAndIndex(key).fold(this)((oldV, oldI) =>
      val (v, i) = f(oldV, oldI)
      inserted(key, v, i)
    )

  def updatedValueWith(key: K, f: A => A): KeyedIndexedList[K, A] =
    updatedWith(key, (v, i) => (f(v), i))

object KeyedIndexedList:
  def empty[K, A]: KeyedIndexedList[K, A] = KeyedIndexedList[K, A](TreeSeqMap.empty)

  def fromList[K, A](list: List[A], getKey: A => K): KeyedIndexedList[K, A] =
    KeyedIndexedList(TreeSeqMap.from(list.distinctBy(getKey).zipWithIndex.map { case (a, idx) =>
      (getKey(a), (a, idx))
    }))

  def unsafeFromTreeSeqMap[K, A](list: TreeSeqMap[K, (A, Int)]): KeyedIndexedList[K, A] =
    KeyedIndexedList(list)

  def value[A]: Lens[(A, Int), A] = Focus[(A, Int)](_._1)

  // Should we have an Order?
  given eqKeyedIndexedList[K, A: Eq]: Eq[KeyedIndexedList[K, A]] =
    Eq.by(_.list: Map[K, (A, Int)])

  given keyIndexedListAt[K, A]: At[KeyedIndexedList[K, A], K, Option[(A, Int)]] =
    At(i =>
      Lens((_: KeyedIndexedList[K, A]).getValueAndIndex(i))(optV =>
        kil => optV.fold(kil.removed(i))((v, idx) => kil.inserted(i, v, idx))
      )
    )

  given treeSeqMapIndex[K, A]: Index[KeyedIndexedList[K, A], K, (A, Int)] = fromAt

  given keyIndexedListFilterIndex[K, A]: FilterIndex[KeyedIndexedList[K, A], K, (A, Int)] =
    new FilterIndex[KeyedIndexedList[K, A], K, (A, Int)]:
      import cats.syntax.applicative._
      import cats.syntax.functor._

      def filterIndex(predicate: K => Boolean) =
        new Traversal[KeyedIndexedList[K, A], (A, Int)] {
          def modifyA[F[_]: Applicative](
            f: ((A, Int)) => F[(A, Int)]
          )(s: KeyedIndexedList[K, A]): F[KeyedIndexedList[K, A]] =
            s.list.toList
              .traverse { case (k, (v, i)) =>
                (if (predicate(k)) f(v, i) else (v, i).pure[F]).tupleLeft(k)
              }
              .map(list =>
                KeyedIndexedList.unsafeFromTreeSeqMap(
                  TreeSeqMap.from(
                    list.sortBy { case (_, (_, i)) => i }
                  )
                )
              )
        }
