// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.undo

import utest._

import cats.effect.IO
import cats.implicits._
import cats.effect.concurrent.Ref
import monocle.Iso
import scala.concurrent._
import ExecutionContext.Implicits.global
import explore.components.undo.ListItem

object UndoerSpec extends TestSuite {

  def id[A] = Iso.id[A].asLens.asGetter

  def eqByRef[A]: A => A => Boolean = (a1: A) => (a2: A) => a1 == a2

  def listItemEqByRef[F[_], A] = ListItem[F, A, A](eqByRef) _

  val tests = Tests {
    test("Undo") {
      val io =
        for {
          model    <- Ref[IO].of(0)
          undoable <- TestUndoable(model)
          _        <- undoable.set(id[Int], model.set, 1)
          _        <- undoable.set(id[Int], model.set, 2)
          _        <- undoable.undo
          v        <- undoable.get
        } yield v
      io.unsafeToFuture().map(result => assert(result == 1))
    }

    test("Redo") {
      val io =
        for {
          model    <- Ref[IO].of(0)
          undoable <- TestUndoable(model)
          _        <- undoable.set(id[Int], model.set, 1)
          _        <- undoable.set(id[Int], model.set, 2)
          _        <- undoable.set(id[Int], model.set, 3)
          _        <- undoable.undo
          _        <- undoable.undo
          _        <- undoable.redo
          v        <- undoable.get
        } yield v
      io.unsafeToFuture().map(result => assert(result == 2))
    }

    test("ListSet") {
      val io =
        for {
          model    <- Ref[IO].of(List(1, 2, 3, 4, 5))
          undoable <- TestUndoable(model)
          item = listItemEqByRef[IO, Int](3)
          _ <- undoable
            .mod[Option[(Int, Int)]](item.getter, item.setter(model.update), item.setPos(8))
          v <- undoable.get
        } yield v
      io.unsafeToFuture().map(result => assert(result == List(1, 2, 4, 5, 3)))
    }

    test("ListUndo") {
      val io =
        for {
          model    <- Ref[IO].of(List(1, 2, 3, 4, 5))
          undoable <- TestUndoable(model)
          item = listItemEqByRef[IO, Int](3)
          _ <- undoable.mod(item.getter, item.setter(model.update), item.setPos(8))
          _ <- undoable.undo
          v <- undoable.get
        } yield v
      io.unsafeToFuture().map(result => assert(result == List(1, 2, 3, 4, 5)))
    }

    test("ListDelete") {
      val io =
        for {
          model    <- Ref[IO].of(List(1, 2, 3, 4, 5))
          undoable <- TestUndoable(model)
          item = listItemEqByRef[IO, Int](3)
          _ <- undoable.mod(item.getter, item.setter(model.update), item.delete)
          v <- undoable.get
        } yield v
      io.unsafeToFuture().map(result => assert(result == List(1, 2, 4, 5)))
    }

    test("ListUndoDelete") {
      val io =
        for {
          model    <- Ref[IO].of(List(1, 2, 3, 4, 5))
          undoable <- TestUndoable(model)
          item = listItemEqByRef[IO, Int](3)
          _ <- undoable.mod(item.getter, item.setter(model.update), item.delete)
          _ <- undoable.undo
          v <- undoable.get
        } yield v
      io.unsafeToFuture().map(result => assert(result == List(1, 2, 3, 4, 5)))
    }

    test("ListInsert") {
      val io =
        for {
          model    <- Ref[IO].of(List(1, 2, 3, 4, 5))
          undoable <- TestUndoable(model)
          item = listItemEqByRef[IO, Int](8)
          _ <- undoable.mod(item.getter, item.setter(model.update), item.upsert(8, 3))
          v <- undoable.get
        } yield v
      io.unsafeToFuture().map(result => assert(result == List(1, 2, 3, 8, 4, 5)))
    }

    test("ListUndoInsert") {
      val io =
        for {
          model    <- Ref[IO].of(List(1, 2, 3, 4, 5))
          undoable <- TestUndoable(model)
          item = listItemEqByRef[IO, Int](8)
          _ <- undoable.mod(item.getter, item.setter(model.update), item.upsert(8, 3))
          _ <- undoable.undo
          v <- undoable.get
        } yield v
      io.unsafeToFuture().map(result => assert(result == List(1, 2, 3, 4, 5)))
    }
  }
}
