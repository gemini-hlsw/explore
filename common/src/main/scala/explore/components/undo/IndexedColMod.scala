// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components.undo

import monocle._
import cats.implicits._

trait IndexedColMod[F[_], Col[_], Idx, A, Id] {
  def getById(col: Col[A], id: Id): Option[(A, Idx)]

  def removeWithIdx(col: Col[A], idx: Idx): Col[A]

  def insertWithIdx(col: Col[A], idx: Idx, a: A): Col[A]

  case class ItemWithId(id: Id) {
    val getter: Getter[Col[A], Option[(A, Idx)]] =
      Getter[Col[A], Option[(A, Idx)]](col => getById(col, id))

    protected def idxSetter: Setter[Col[A], Option[(A, Idx)]] =
      Setter[Col[A], Option[(A, Idx)]] {
        // If element is still in the collection, just modify it's location.
        // If it isn't but now it has to be, reinstate it.
        mod => col =>
          val oldElemAndIndex = getter.get(col)
          val (baseCol, oldElem) =
            oldElemAndIndex
              .fold((col, none[A])) {
                case (elem, idx) =>
                  (removeWithIdx(col, idx), elem.some)
              }
          val newElemAndIndex = mod(oldElemAndIndex)
          newElemAndIndex.fold(baseCol) {
            case (newElem, idx) =>
              insertWithIdx(baseCol, idx, oldElem.getOrElse(newElem))
          }
      }

    def setter(
      mod: (Col[A] => Col[A]) => F[Unit]
    ): Option[(A, Idx)] => F[Unit] =
      elemAndIndex => mod(idxSetter.set(elemAndIndex))

    def modIdx(f: Idx => Idx): Option[(A, Idx)] => Option[(A, Idx)] =
      _.map { case (value, idx) => (value, f(idx)) }

    def setIdx(idx: Idx): Option[(A, Idx)] => Option[(A, Idx)] =
      modIdx(_ => idx)

    val delete: Option[(A, Idx)] => Option[(A, Idx)] =
      _ => none

    // upsert is unsafe. There's no guarantee that idF(a) == true
    // We could check and show a warning?
    def upsert(a: A, idx: Idx): Option[(A, Idx)] => Option[(A, Idx)] =
      _ => (a, idx).some
  }
}

class ListMod[F[_], A, Id](idF: Id => A => Boolean) extends IndexedColMod[F, List, Int, A, Id] {

  override def getById(list: List[A], id: Id): Option[(A, Int)] =
    list.indexWhere(idF(id)).some.filter(_ >= 0).map(i => (list(i), i))

  override def removeWithIdx(list: List[A], idx: Int): List[A] =
    list.take(idx) ++ list.drop(idx + 1)

  override def insertWithIdx(list: List[A], idx: Int, a: A): List[A] =
    list.take(idx) ++ (a +: list.drop(idx))
}
