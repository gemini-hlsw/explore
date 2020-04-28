// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components.undo

import monocle._
import cats.implicits._

object ListItem {
  def indexGet[A](idF: A => Boolean): Getter[List[A], Option[(A, Int)]] =
    Getter[List[A], Option[(A, Int)]] { list =>
      list.indexWhere(idF).some.filter(_ >= 0).map(i => (list(i), i))
    }

  def indexSet[A](idF: A => Boolean): Setter[List[A], Option[(A, Int)]] = {
    val getter = indexGet(idF)

    Setter[List[A], Option[(A, Int)]] {
      // If element is still in the list, just modify it's location.
      // If it isn't but now it has to be, reinstate it.
      mod => list =>
        val oldElemAndIndex = getter.get(list)
        val (baseList, oldElem) =
          oldElemAndIndex
            .fold((list, none[A])) {
              case (elem, idx) =>
                (list.take(idx) ++ list.drop(idx + 1), elem.some)
            }
        val newElemAndIndex = mod(oldElemAndIndex)
        newElemAndIndex.fold(baseList) {
          case (newElem, idx) =>
            baseList.take(idx) ++ (oldElem.getOrElse(newElem) +: baseList.drop(idx))
        }
    }
  }
}

case class ListItem[F[_], A, Id](idF: Id => A => Boolean)(id: Id) {
  val getter: Getter[List[A], Option[(A, Int)]] = ListItem.indexGet[A](idF(id))

  def setter(
    mod: (List[A] => List[A]) => F[Unit]
  ): Option[(A, Int)] => F[Unit] =
    elemAndIndex => mod(ListItem.indexSet[A](idF(id)).set(elemAndIndex))

  def modPos(f: Int => Int): Option[(A, Int)] => Option[(A, Int)] =
    _.map { case (value, idx) => (value, f(idx)) }

  def setPos(idx: Int): Option[(A, Int)] => Option[(A, Int)] =
    modPos(_ => idx)

  val delete: Option[(A, Int)] => Option[(A, Int)] =
    _ => none

  // upsert is unsafe. There's no guarantee that idF(a) == true
  // We could check and show a warning?
  def upsert(a: A, idx: Int): Option[(A, Int)] => Option[(A, Int)] =
    _ => (a, idx).some
}
