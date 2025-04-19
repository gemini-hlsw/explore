// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.undo

import cats.syntax.all.*
import explore.optics.Adjuster
import explore.optics.GetAdjust
import monocle.*
import monocle.std.option.some
import mouse.boolean.*

trait IndexedCollMod[Coll[_, _], Idx, A, N[_], K] { // N = Type of internal Node containing A. Can be Id for just A.
  protected val keyLens: Lens[A, K]

  protected val valueLens: Lens[N[A], A]
  protected val pureNode: A => N[A]

  type ElemWithIndex    = (N[A], Idx)
  type ElemWithIndexOpt = Option[ElemWithIndex]
  type Operation        = ElemWithIndexOpt => ElemWithIndexOpt
  type Collection       = Coll[K, A]

  private def first: Lens[(N[A], Idx), N[A]] = Focus[(N[A], Idx)](_._1)

  protected lazy val ElemWithIndexOptKey: Optional[ElemWithIndexOpt, K] =
    some.andThen(first).andThen(valueLens).andThen(keyLens)

  protected def getterForKey(key: K): Getter[Collection, ElemWithIndexOpt]

  def removeWithKey(col: Collection, key: K): Collection

  def insertWithIdx(col: Collection, idx: Idx, node: N[A]): Collection

  protected def adjusterForKey(
    key:      K,
    getter:   Getter[Collection, ElemWithIndexOpt],
    preserve: Boolean // If true, won't modify an existing element, just its location. Deletion is still possible.
  ): Adjuster[Collection, ElemWithIndexOpt] =
    Adjuster[Collection, ElemWithIndexOpt] { mod => coll =>
      val oldElemAndIndex        = getter.get(coll)
      val newElemAndIndex        = mod(oldElemAndIndex)
      val newElemAndIndexWithKey =
        ElemWithIndexOptKey.replace(key)(newElemAndIndex) // Reinstate key

      val baseColl = removeWithKey(coll, key)

      newElemAndIndexWithKey.fold(baseColl) { case (newElem, idx) =>
        insertWithIdx(baseColl,
                      idx,
                      preserve.fold(oldElemAndIndex.map(_._1).getOrElse(newElem), newElem)
        )
      }
    }

  def withKey(key: K): GetAdjust[Collection, ElemWithIndexOpt] = {
    val getter   = getterForKey(key)
    val adjuster = adjusterForKey(key, getter, preserve = false)
    GetAdjust(getter, adjuster)
  }

  // Start Element Operations
  // Key is reinstated (it can't be modified.)
  def mod(f: A => A): Operation =
    _.map { case (node, idx) =>
      (valueLens.modify(value => keyLens.replace(keyLens.get(value))(f(value)))(node), idx)
    }

  // Key is reinstated (it can't be modified.)
  def set(a: A): Operation =
    mod(_ => a)

  val delete: Operation =
    _ => none

  // If updating, key is reinstated (it can't be modified.)
  def upsert(a: A, idx: Idx): Operation =
    _.map { case (node, _) =>
      (valueLens.modify(value => keyLens.replace(keyLens.get(value))(a))(node), idx)
    }.orElse((pureNode(a), idx).some)

  // End Element Operations

  object pos {
    def withKey(key: K): GetAdjust[Collection, ElemWithIndexOpt] = {
      val getter   = getterForKey(key)
      val adjuster = adjusterForKey(key, getter, preserve = true)
      GetAdjust(getter, adjuster)
    }

    // Start Position Operations
    def mod(f: Idx => Idx): Operation =
      _.map { case (value, idx) => (value, f(idx)) }

    def set(idx: Idx): Operation =
      mod(_ => idx)
    // End Position Operations
  }
}
