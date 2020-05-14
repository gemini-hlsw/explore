// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.undo

import cats.implicits._
import monocle._
import mouse.boolean._

trait IndexedColMod[F[_], Col[_], Idx, A, Id] {
  protected val idLens: Lens[A, Id]

  type ElemWithIndex = Option[(A, Idx)]
  type Operation     = ElemWithIndex => ElemWithIndex

  protected def getterForId(id: Id): Getter[Col[A], ElemWithIndex]

  def removeWithIdx(col: Col[A], idx: Idx): Col[A]

  def insertWithIdx(col: Col[A], idx: Idx, a: A): Col[A]

  protected def setterForId(
    getter:   Getter[Col[A], ElemWithIndex],
    preserve: Boolean // If true, won't modify an existing element, just its location. Deletion is still possible.
  ): Setter[Col[A], ElemWithIndex] =
    Setter[Col[A], ElemWithIndex] { mod => col =>
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
          insertWithIdx(baseCol, idx, preserve.fold(oldElem.getOrElse(newElem), newElem))
      }
    }

  def withId(id: Id): GetSet[Col[A], ElemWithIndex] = {
    val getter = getterForId(id)
    val setter = setterForId(getter, preserve = false)
    GetSet(getter, setter)
  }

  // Start Element Operations
  // Id is reinstated (it can't be modified.)
  def mod(f: A => A): Operation =
    _.map { case (value, idx) => (idLens.set(idLens.get(value))(f(value)), idx) }

  // Id is reinstated (it can't be modified.)
  def set(a: A): Operation      =
    mod(_ => a)

  val delete: Operation =
    _ => none

  // If updating, Id is reinstated (it can't be modified.)
  def upsert(a: A, idx: Idx): Operation =
    _.map { case (value, _) => (idLens.set(idLens.get(value))(a), idx) }.orElse((a, idx).some)
  // End Element Operations

  object pos {
    def withId(id: Id): GetSet[Col[A], ElemWithIndex] = {
      val getter = getterForId(id)
      val setter = setterForId(getter, preserve = true)
      GetSet(getter, setter)
    }

    // Start Position Operations
    def mod(f:   Idx => Idx): Operation =
      _.map { case (value, idx) => (value, f(idx)) }

    def set(idx: Idx): Operation        =
      mod(_ => idx)
    // End Position Operations
  }
}
