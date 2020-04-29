// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.undo

import utest._

import cats.effect.IO
import cats.implicits._
import cats.effect.concurrent.Ref
import monocle.Iso
import cats.kernel.Eq
import explore.components.undo._

object UndoerSpec extends TestSuite {

  def id[A] = Iso.id[A].asLens.asGetter

  def eqByIdEq[A, Id: Eq](getId: A => Id): Id => A => Boolean =
    (id: Id) => (a: A) => Eq[Id].eqv(id, getId(a))

  class ListModByIdEq[F[_], A, Id: Eq](getId: A => Id)
      extends ListMod[F, A, Id](eqByIdEq[A, Id](getId))

  class ListModIdentityId[F[_], A: Eq] extends ListModByIdEq[F, A, A](identity)

  val listIntMod = new ListModIdentityId[IO, Int]

  val tests = Tests {
    test("UndoAndRedo") {
      (for {
        model    <- Ref[IO].of(0)
        undoable <- TestUndoable(model)
        _        <- undoable.set(id[Int], model.set, 1)
        _        <- undoable.set(id[Int], model.set, 2)
        _        <- undoable.set(id[Int], model.set, 3)
        _        <- undoable.get.map(v => assert(v == 3))
        _        <- undoable.undo
        _        <- undoable.get.map(v => assert(v == 2))
        _        <- undoable.undo
        _        <- undoable.get.map(v => assert(v == 1))
        _        <- undoable.redo
        _        <- undoable.get.map(v => assert(v == 2))
        _        <- undoable.redo
        _        <- undoable.get.map(v => assert(v == 3))
      } yield ()).unsafeToFuture()
    }

    test("ListModPosUndoRedo") {
      (for {
        model    <- Ref[IO].of(List(1, 2, 3, 4, 5))
        undoable <- TestUndoable(model)
        item = listIntMod.ItemWithId(3)
        _ <- undoable.mod(item.getter, item.setter(model.update), item.setIdx(8))
        _ <- undoable.get.map(v => assert(v == List(1, 2, 4, 5, 3)))
        _ <- undoable.undo
        _ <- undoable.get.map(v => assert(v == List(1, 2, 3, 4, 5)))
        _ <- undoable.redo
        _ <- undoable.get.map(v => assert(v == List(1, 2, 4, 5, 3)))
      } yield ()).unsafeToFuture()
    }

    test("ListDeleteUndoRedo") {
      (for {
        model    <- Ref[IO].of(List(1, 2, 3, 4, 5))
        undoable <- TestUndoable(model)
        item = listIntMod.ItemWithId(3)
        _ <- undoable.mod(item.getter, item.setter(model.update), item.delete)
        _ <- undoable.get.map(v => assert(v == List(1, 2, 4, 5)))
        _ <- undoable.undo
        _ <- undoable.get.map(v => assert(v == List(1, 2, 3, 4, 5)))
        _ <- undoable.redo
        _ <- undoable.get.map(v => assert(v == List(1, 2, 4, 5)))
      } yield ()).unsafeToFuture()
    }

    test("ListInsertUndoRedo") {
      (for {
        model    <- Ref[IO].of(List(1, 2, 3, 4, 5))
        undoable <- TestUndoable(model)
        item = listIntMod.ItemWithId(8)
        _ <- undoable.mod(item.getter, item.setter(model.update), item.upsert(8, 3))
        _ <- undoable.get.map(v => assert(v == List(1, 2, 3, 8, 4, 5)))
        _ <- undoable.undo
        _ <- undoable.get.map(v => assert(v == List(1, 2, 3, 4, 5)))
        _ <- undoable.redo
        _ <- undoable.get.map(v => assert(v == List(1, 2, 3, 8, 4, 5)))
      } yield ()).unsafeToFuture()
    }

    case class V(id: Int, s: String)
    object V {
      def apply(id: Int): V = V(id, id.toString)
    }

    val vListMod = new ListModByIdEq[IO, V, Int](_.id)

    test("ListObjModPosUndoRedo") {
      (for {
        model    <- Ref[IO].of(List(V(1), V(2), V(3), V(4), V(5)))
        undoable <- TestUndoable(model)
        item = vListMod.ItemWithId(3)
        _ <- undoable.mod(item.getter, item.setter(model.update), item.setIdx(8))
        _ <- undoable.get.map(v =>
          assert(v == List(V(1, "1"), V(2, "2"), V(4, "4"), V(5, "5"), V(3, "3")))
        )
        _ <- model.update(l => l.init :+ l.last.copy(s = "three")) // External modification, before undo
        _ <- undoable.get.map(v =>
          assert(v == List(V(1, "1"), V(2, "2"), V(4, "4"), V(5, "5"), V(3, "three")))
        )
        _ <- undoable.undo
        _ <- undoable.get.map(v =>
          assert(v == List(V(1, "1"), V(2, "2"), V(3, "three"), V(4, "4"), V(5, "5")))
        )
        _ <- undoable.redo
        _ <- undoable.get.map(v =>
          assert(v == List(V(1, "1"), V(2, "2"), V(4, "4"), V(5, "5"), V(3, "three")))
        )
        _ <- undoable.undo
        _ <- model.update(l => l.take(2) ++ (l(2).copy(s = "tres") +: l.drop(3))) // External modification, before redo
        _ <- undoable.get.map(v =>
          assert(v == List(V(1, "1"), V(2, "2"), V(3, "tres"), V(4, "4"), V(5, "5")))
        )
        _ <- undoable.redo
        _ <- undoable.get.map(v =>
          assert(v == List(V(1, "1"), V(2, "2"), V(4, "4"), V(5, "5"), V(3, "tres")))
        )
      } yield ()).unsafeToFuture()
    }
  }
}
