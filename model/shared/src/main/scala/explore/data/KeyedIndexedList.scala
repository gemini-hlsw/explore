// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.data

import cats.Applicative
import cats.Eq
import cats.Order
import cats.Order.given
import cats.syntax.all.given
import eu.timepit.refined.cats.*
import eu.timepit.refined.numeric.NonNegative
import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.refined.*
import monocle.Focus
import monocle.Lens
import monocle.Traversal
import monocle.function.At
import monocle.function.FilterIndex
import monocle.function.Index
import monocle.function.Index.fromAt

import scala.collection.immutable.TreeSeqMap

// Each element has a unique Key.
// Efficient loookup of elements and positions by Key.
case class KeyedIndexedList[K, A] private (private val list: TreeSeqMap[K, (A, NonNegInt)]):
  def getValueAndIndex(key: K): Option[(A, NonNegInt)] = list.get(key)
  def getValue(key:         K): Option[A]              = list.get(key).map(_._1)
  def getIndex(key:         K): Option[NonNegInt]      = list.get(key).map(_._2)

  def toIndexedList: List[(A, NonNegInt)] = list.values.toList

  def toList: List[A] = toIndexedList.map(_._1)

  def toMap: Map[K, A] = list.map { case (k, (v, _)) => k -> v }

  def collect[B](pf: PartialFunction[(K, (A, NonNegInt)), B]): List[B] =
    list.collect(pf).toList

  def length: NonNegInt = NonNegInt.unsafeFrom(list.size)

  def empty: Boolean = length == 0.refined[NonNegative]

  def nonEmpty: Boolean = !empty

  def removed(key: K): KeyedIndexedList[K, A] =
    getIndex(key)
      .fold(this): idx =>
        KeyedIndexedList.unsafeFromTreeSeqMap:
          list
            .removed(key)
            .map:
              case (key, (a, i)) if i < idx     => (key, (a, i))
              case (key, (a, i)) /*if i > idx*/ => (key, (a, NonNegInt.unsafeFrom(i.value - 1)))

  def contains(key: K): Boolean = list.contains(key)

  def exists(p: A => Boolean): Boolean = list.values.exists { case (a, _) => p(a) }

  def take(n: NonNegInt): KeyedIndexedList[K, A] =
    KeyedIndexedList.unsafeFromTreeSeqMap(list.take(n.value))

  def drop(n: NonNegInt): KeyedIndexedList[K, A] =
    KeyedIndexedList.unsafeFromTreeSeqMap:
      list
        .collect:
          case (id, (a, i)) if i >= n => (id, (a, NonNegInt.unsafeFrom(i.value - n.value)))

  def inserted(key: K, elem: A, idx: NonNegInt): KeyedIndexedList[K, A] = {
    val fixedIdx: NonNegInt = idx match
      case i if i > length    => length
      case i if i < 0.refined => 0.refined
      case i                  => i

    val baseList: TreeSeqMap[K, (A, NonNegInt)] = removed(key).list

    val (front, back): (TreeSeqMap[K, (A, NonNegInt)], TreeSeqMap[K, (A, NonNegInt)]) =
      baseList.splitAt(fixedIdx.value)

    KeyedIndexedList.unsafeFromTreeSeqMap:
      (front + ((key, (elem, fixedIdx)))) ++
        back.map { case (k, (e, i)) =>
          (k, (e, NonNegInt.unsafeFrom(i.value + 1)))
        }
  }

  def updated(key: K, value: A, idx: NonNegInt): KeyedIndexedList[K, A] =
    if (contains(key))
      inserted(key, value, idx)
    else
      this

  def updatedWith(key: K, f: (A, NonNegInt) => (A, NonNegInt)): KeyedIndexedList[K, A] =
    getValueAndIndex(key).fold(this): (oldV, oldI) =>
      val (v, i) = f(oldV, oldI)
      inserted(key, v, i)

  def updatedValueWith(key: K, f: A => A): KeyedIndexedList[K, A] =
    updatedWith(key, (v, i) => (f(v), i))

  // WARNING - Do not use to update the part of the value used as a key
  def unsafeMapValues(f: A => A): KeyedIndexedList[K, A] =
    KeyedIndexedList(TreeSeqMap.from(list.map { case (k, (a, idx)) => (k, (f(a), idx)) }))

object KeyedIndexedList:
  def empty[K, A]: KeyedIndexedList[K, A] = KeyedIndexedList[K, A](TreeSeqMap.empty)

  def fromList[K, A](list: List[A], getKey: A => K): KeyedIndexedList[K, A] =
    KeyedIndexedList:
      TreeSeqMap.from:
        list
          .distinctBy(getKey)
          .zipWithIndex
          .map: (a, idx) =>
            (getKey(a), (a, NonNegInt.unsafeFrom(idx)))

  def unsafeFromTreeSeqMap[K, A](list: TreeSeqMap[K, (A, NonNegInt)]): KeyedIndexedList[K, A] =
    KeyedIndexedList(list)

  def value[A]: Lens[(A, NonNegInt), A] = Focus[(A, NonNegInt)](_._1)

  // Should we have an Order?
  given eqKeyedIndexedList[K, A: Eq]: Eq[KeyedIndexedList[K, A]] =
    Eq.by(_.list: Map[K, (A, NonNegInt)])

  given keyedIndexedListAt[K, A]: At[KeyedIndexedList[K, A], K, Option[(A, NonNegInt)]] =
    At(i =>
      Lens((_: KeyedIndexedList[K, A]).getValueAndIndex(i))(optV =>
        kil => optV.fold(kil.removed(i))((v, idx) => kil.inserted(i, v, idx))
      )
    )

  given keyedIndexedIndex[K, A]: Index[KeyedIndexedList[K, A], K, (A, NonNegInt)] = fromAt

  given keyedIndexedListFilterIndex[K, A]: FilterIndex[KeyedIndexedList[K, A], K, (A, NonNegInt)] =
    new FilterIndex[KeyedIndexedList[K, A], K, (A, NonNegInt)]:
      import cats.syntax.applicative._
      import cats.syntax.functor._

      def filterIndex(predicate: K => Boolean) =
        new Traversal[KeyedIndexedList[K, A], (A, NonNegInt)] {
          def modifyA[F[_]: Applicative](
            f: ((A, NonNegInt)) => F[(A, NonNegInt)]
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
