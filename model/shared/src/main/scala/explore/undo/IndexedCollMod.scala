// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.undo

import cats.syntax.all._
import explore.optics.Adjuster
import explore.optics.GetAdjust
import monocle._
import monocle.function.Field1.first
import monocle.std.option.some
import mouse.boolean._

trait IndexedCollMod[F[_], Coll[_, _], Idx, A, N[_], K] { // N = Type of internal Node containing A. Can be Id for just A.
  protected val keyLens: Lens[A, K]

  protected val valueLens: Lens[N[A], A]
  protected val pureNode: A => N[A]

  type ElemWithIndex = Option[(N[A], Idx)]
  type Operation     = ElemWithIndex => ElemWithIndex
  type Collection    = Coll[K, A]

  protected lazy val elemWithIndexKey: Optional[ElemWithIndex, K] =
    some.composeLens(first[(N[A], Idx), N[A]]).composeLens(valueLens).composeLens(keyLens)

  protected def getterForKey(key: K): Getter[Collection, ElemWithIndex]

  def removeWithKey(col: Collection, key: K): Collection

  def insertWithIdx(col: Collection, idx: Idx, node: N[A]): Collection

  protected def adjusterForKey(
    key:      K,
    getter:   Getter[Collection, ElemWithIndex],
    preserve: Boolean // If true, won't modify an existing element, just its location. Deletion is still possible.
  ): Adjuster[Collection, ElemWithIndex] =
    Adjuster[Collection, ElemWithIndex] { mod => coll =>
      val oldElemAndIndex        = getter.get(coll)
      val newElemAndIndex        = mod(oldElemAndIndex)
      val newElemAndIndexWithKey = elemWithIndexKey.set(key)(newElemAndIndex) // Reinstate key

      val baseColl = removeWithKey(coll, key)

      newElemAndIndexWithKey.fold(baseColl) { case (newElem, idx) =>
        insertWithIdx(baseColl,
                      idx,
                      preserve.fold(oldElemAndIndex.map(_._1).getOrElse(newElem), newElem)
        )
      }
    }

  def withKey(key: K): GetAdjust[Collection, ElemWithIndex] = {
    val getter   = getterForKey(key)
    val adjuster = adjusterForKey(key, getter, preserve = false)
    GetAdjust(getter, adjuster)
  }

  // Start Element Operations
  // Key is reinstated (it can't be modified.)
  def mod(f: A => A): Operation =
    _.map { case (node, idx) =>
      (valueLens.modify(value => keyLens.set(keyLens.get(value))(f(value)))(node), idx)
    }

  // Key is reinstated (it can't be modified.)
  def set(a: A): Operation =
    mod(_ => a)

  val delete: Operation =
    _ => none

  // If updating, key is reinstated (it can't be modified.)
  def upsert(a: A, idx: Idx): Operation =
    _.map { case (node, _) =>
      (valueLens.modify(value => keyLens.set(keyLens.get(value))(a))(node), idx)
    }.orElse((pureNode(a), idx).some)

  // End Element Operations

  object pos {
    def withKey(key: K): GetAdjust[Collection, ElemWithIndex] = {
      val getter   = getterForKey(key)
      val adjuster = adjusterForKey(key, getter, preserve = true)
      GetAdjust(getter, adjuster)
    }

    // Start Position Operations
    def mod(f:   Idx => Idx): Operation =
      _.map { case (value, idx) => (value, f(idx)) }

    def set(idx: Idx): Operation        =
      mod(_ => idx)
    // End Position Operations
  }
}
