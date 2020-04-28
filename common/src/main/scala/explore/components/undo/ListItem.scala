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
        val baseList =
          oldElemAndIndex
            .fold(list) {
              case (_, idx) =>
                list.take(idx) ++ list.drop(idx + 1)
            }
        val newElemAndIndex = mod(oldElemAndIndex)
        newElemAndIndex.fold(baseList) {
          case (element, idx) =>
            baseList.take(idx) ++ (element +: baseList.drop(idx))
        }
    }
  }
}

case class ListItem[F[_], A, Id](idF: Id => A => Boolean)(id: Id) {
  val getter: Getter[List[A], Option[(A, Int)]] = ListItem.indexGet[A](idF(id))

  def setter(
    mod: (List[A] => List[A]) => F[Unit]
  ): Option[(A, Int)] => F[Unit] =
    valIdxOpt => mod(ListItem.indexSet[A](idF(id)).set(valIdxOpt))

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
