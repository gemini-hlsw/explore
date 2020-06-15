// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.undo

import cats.implicits._
import monocle._
import mouse.boolean._

trait IndexedCollMod[F[_], Coll[_], Idx, A, N[_], Id] { // N = Type of internal Node containing A. Can be Id for just A.
  protected val idLens: Lens[A, Id]

  protected val valueLens: Lens[N[A], A]
  protected val pureNode: A => N[A]

  type ElemWithIndex = Option[(N[A], Idx)]
  type Operation     = ElemWithIndex => ElemWithIndex

  protected def getterForId(id: Id): Getter[Coll[A], ElemWithIndex]

  def removeWithIdx(col: Coll[A], idx: Idx): Coll[A]

  def insertWithIdx(col: Coll[A], idx: Idx, node: N[A]): Coll[A]

  protected def setterForId(
    getter:   Getter[Coll[A], ElemWithIndex],
    preserve: Boolean // If true, won't modify an existing element, just its location. Deletion is still possible.
  ): Setter[Coll[A], ElemWithIndex] =
    Setter[Coll[A], ElemWithIndex] { mod => coll =>
      val oldElemAndIndex     = getter.get(coll)
      val (baseColl, oldElem) =
        oldElemAndIndex
          .fold((coll, none[N[A]])) {
            case (elem, idx) =>
              (removeWithIdx(coll, idx), elem.some)
          }
      val newElemAndIndex     = mod(oldElemAndIndex)
      newElemAndIndex.fold(baseColl) {
        case (newElem, idx) =>
          insertWithIdx(baseColl, idx, preserve.fold(oldElem.getOrElse(newElem), newElem))
      }
    }

  def withId(id: Id): GetSet[Coll[A], ElemWithIndex] = {
    val getter = getterForId(id)
    val setter = setterForId(getter, preserve = false)
    GetSet(getter, setter)
  }

  // Start Element Operations
  // Id is reinstated (it can't be modified.)
  def mod(f: A => A): Operation =
    _.map {
      case (node, idx) =>
        (valueLens.modify(value => idLens.set(idLens.get(value))(f(value)))(node), idx)
    }

  // Id is reinstated (it can't be modified.)
  def set(a: A): Operation =
    mod(_ => a)

  val delete: Operation =
    _ => none

  // If updating, Id is reinstated (it can't be modified.)
  def upsert(a: A, idx: Idx): Operation =
    _.map {
      case (node, _) => (valueLens.modify(value => idLens.set(idLens.get(value))(a))(node), idx)
    }.orElse((pureNode(a), idx).some)
  // End Element Operations

  object pos {
    def withId(id: Id): GetSet[Coll[A], ElemWithIndex] = {
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
