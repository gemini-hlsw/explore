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

object UndoerSpec extends TestSuite {

  def id[A] = Iso.id[A].asLens.asGetter

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
        } yield (v)
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
        } yield (v)
      io.unsafeToFuture().map(result => assert(result == 2))
    }
  }
}
