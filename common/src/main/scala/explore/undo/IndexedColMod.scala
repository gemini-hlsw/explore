// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.undo

import monocle._
import cats.implicits._
import mouse.boolean._
import monocle.function.At

trait IndexedColMod[F[_], Col[_], Idx, A, Id] extends At[Col[A], Id, Option[(A, Idx)]] {
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

  // In this version of `at`, the index is actually an id.
  override def at(id: Id): Lens[Col[A], ElemWithIndex] = {
    val getter = getterForId(id)
    val setter = setterForId(getter, preserve = false)
    Lens(getter.get)(setter.set)
  }

  // Start Element Operations
  // Id is reinstated in order to preserve Lens laws.
  def mod(f: A => A): Operation =
    _.map { case (value, idx) => (idLens.set(idLens.get(value))(f(value)), idx) }

  def set(a: A): Operation =
    mod(_ => a)

  val delete: Operation =
    _ => none

  // If updating, Id is reinstated in order to preserve Lens laws.
  def upsert(a: A, idx: Idx): Operation =
    _.map { case (value, _) => (idLens.set(idLens.get(value))(a), idx) }.orElse((a, idx).some)
  // End Element Operations

  object pos extends At[Col[A], Id, Option[(A, Idx)]] {
    override def at(id: Id): Lens[Col[A], ElemWithIndex] = {
      val getter = getterForId(id)
      val setter = setterForId(getter, preserve = true)
      Lens(getter.get)(setter.set)
    }

    // Start Position Operations
    def mod(f:          Idx => Idx): Operation           =
      _.map { case (value, idx) => (value, f(idx)) }

    def set(idx:        Idx): Operation                  =
      mod(_ => idx)
    // End Position Operations
  }
}
