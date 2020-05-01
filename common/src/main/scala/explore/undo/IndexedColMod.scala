// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.undo

import monocle._
import cats.implicits._
import monocle.function.At

trait IndexedColMod[F[_], Col[_], Idx, A, Id] extends At[Col[A], Id, Option[(A, Idx)]] {
  type ElemWithIndex = Option[(A, Idx)]
  type Operation     = ElemWithIndex => ElemWithIndex

  protected def getterForId(id: Id): Getter[Col[A], ElemWithIndex]

  def removeWithIdx(col: Col[A], idx: Idx): Col[A]

  def insertWithIdx(col: Col[A], idx: Idx, a: A): Col[A]

  protected def setterForId(
    getter: Getter[Col[A], ElemWithIndex]
  ): Setter[Col[A], ElemWithIndex] =
    Setter[Col[A], ElemWithIndex] {
      // If element is still in the collection, just modify it's location.
      // If it isn't but now it has to be, reinstate it.
      mod => col =>
        val oldElemAndIndex    = getter.get(col)
        val (baseCol, oldElem) =
          oldElemAndIndex
            .fold((col, none[A])) {
              case (elem, idx) =>
                (removeWithIdx(col, idx), elem.some)
            }
        val newElemAndIndex    = mod(oldElemAndIndex)
        newElemAndIndex.fold(baseCol) {
          case (newElem, idx) =>
            insertWithIdx(baseCol, idx, oldElem.getOrElse(newElem))
        }
    }

  // In this version of `at`, the index is actually an id.
  override def at(id: Id): Lens[Col[A], ElemWithIndex] = {
    val getter = getterForId(id)
    val setter = setterForId(getter)
    Lens(getter.get)(setter.set)
  }

  // Start Operations

  // This is unsafe. There's no guarantee that Id is preserved.
  def mod(f: A => A): Operation =
    _.map { case (value, idx) => (f(value), idx) }

  // This is unsafe. There's no guarantee that Id is preserved.
  def set(a: A): Operation =
    mod(_ => a)

  def modIdx(f:   Idx => Idx): Operation =
    _.map { case (value, idx) => (value, f(idx)) }

  def setIdx(idx: Idx): Operation        =
    modIdx(_ => idx)

  val delete: Operation =
    _ => none

  // This is unsafe. There's no guarantee that Id is preserved.
  def upsert(a: A, idx: Idx): Operation =
    _ => (a, idx).some

  // End Operations
}
