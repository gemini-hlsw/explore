// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.undo

import utest._

import cats.effect.IO
import cats.implicits._
import cats.effect.concurrent.Ref
import monocle.Iso
import cats.kernel.Eq
import explore.undo._
import explore.util.tree._

object UndoerSpec extends TestSuite {

  def id[A] = Iso.id[A].asLens

  def eqByIdEq[A, Id: Eq](getId: A => Id): Id => A => Boolean =
    (id: Id) => (a: A) => Eq[Id].eqv(id, getId(a))

  class ListModByIdEq[F[_], A, Id: Eq](getId: A => Id)
      extends ListMod[F, A, Id](eqByIdEq[A, Id](getId))

  class ListModIdentityId[F[_], A: Eq] extends ListModByIdEq[F, A, A](identity)

  val listIntMod = new ListModIdentityId[IO, Int]

  val tests = Tests {
    test("UndoRedo") {
      (for {
        model    <- Ref[IO].of(0)
        undoable <- TestUndoable(model)
        _        <- undoable.set(id[Int], 1)
        _        <- undoable.set(id[Int], 2)
        _        <- undoable.set(id[Int], 3)
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
        _        <- undoable.mod(listIntMod.at(3), listIntMod.setIdx(8))
        _        <- undoable.get.map(v => assert(v == List(1, 2, 4, 5, 3)))
        _        <- undoable.undo
        _        <- undoable.get.map(v => assert(v == List(1, 2, 3, 4, 5)))
        _        <- undoable.redo
        _        <- undoable.get.map(v => assert(v == List(1, 2, 4, 5, 3)))
      } yield ()).unsafeToFuture()
    }

    test("ListDeleteUndoRedo") {
      (for {
        model    <- Ref[IO].of(List(1, 2, 3, 4, 5))
        undoable <- TestUndoable(model)
        _        <- undoable.mod(listIntMod.at(3), listIntMod.delete)
        _        <- undoable.get.map(v => assert(v == List(1, 2, 4, 5)))
        _        <- undoable.undo
        _        <- undoable.get.map(v => assert(v == List(1, 2, 3, 4, 5)))
        _        <- undoable.redo
        _        <- undoable.get.map(v => assert(v == List(1, 2, 4, 5)))
      } yield ()).unsafeToFuture()
    }

    test("ListInsertUndoRedo") {
      (for {
        model    <- Ref[IO].of(List(1, 2, 3, 4, 5))
        undoable <- TestUndoable(model)
        _        <- undoable.mod(listIntMod.at(8), listIntMod.upsert(8, 3))
        _        <- undoable.get.map(v => assert(v == List(1, 2, 3, 8, 4, 5)))
        _        <- undoable.undo
        _        <- undoable.get.map(v => assert(v == List(1, 2, 3, 4, 5)))
        _        <- undoable.redo
        _        <- undoable.get.map(v => assert(v == List(1, 2, 3, 8, 4, 5)))
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
        _        <- undoable.mod(vListMod.at(3), vListMod.setIdx(8))
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

    class TreeModByIdEq[F[_], A, Id: Eq](getId: A => Id) extends TreeMod[F, A, Id](getId)

    class TreeModIdentityId[F[_], A: Eq] extends TreeModByIdEq[F, A, A](identity)

    val treeIntMod = new TreeModIdentityId[IO, Int]

    test("TreeModPosUndoRedo") {
      (for {
        model <- Ref[IO].of(
          Tree(
            Node(1, Node(2), Node(3)),
            Node(4, Node(5))
          )
        )
        undoable <- TestUndoable(model)
        _        <- undoable.mod(treeIntMod.at(3), treeIntMod.setIdx((4.some, 1)))
        _ <- undoable.get.map(v =>
          assert(
            v ==
              Tree(
                Node(1, Node(2)),
                Node(4, Node(5), Node(3))
              )
          )
        )
        _ <- undoable.undo
        _ <- undoable.get.map(v =>
          assert(
            v ==
              Tree(
                Node(1, Node(2), Node(3)),
                Node(4, Node(5))
              )
          )
        )
        _ <- undoable.redo
        _ <- undoable.get.map(v =>
          assert(
            v ==
              Tree(
                Node(1, Node(2)),
                Node(4, Node(5), Node(3))
              )
          )
        )
      } yield ()).unsafeToFuture()
    }

    test("TreeDeleteUndoRedo") {
      (for {
        model <- Ref[IO].of(
          Tree(
            Node(1, Node(2), Node(3)),
            Node(4, Node(5))
          )
        )
        undoable <- TestUndoable(model)
        _        <- undoable.mod(treeIntMod.at(3), treeIntMod.delete)
        _ <- undoable.get.map(v =>
          assert(
            v ==
              Tree(
                Node(1, Node(2)),
                Node(4, Node(5))
              )
          )
        )
        _ <- undoable.undo
        _ <- undoable.get.map(v =>
          assert(
            v == Tree(
              Node(1, Node(2), Node(3)),
              Node(4, Node(5))
            )
          )
        )
        _ <- undoable.redo
        _ <- undoable.get.map(v =>
          assert(
            v ==
              Tree(
                Node(1, Node(2)),
                Node(4, Node(5))
              )
          )
        )
      } yield ()).unsafeToFuture()
    }

    test("TreeInsertUndoRedo") {
      (for {
        model <- Ref[IO].of(
          Tree(
            Node(1, Node(2), Node(3)),
            Node(4, Node(5))
          )
        )
        undoable <- TestUndoable(model)
        _        <- undoable.mod(treeIntMod.at(8), treeIntMod.upsert(8, (1.some, 8)))
        _ <- undoable.get.map(v =>
          assert(
            v == Tree(
              Node(1, Node(2), Node(3), Node(8)),
              Node(4, Node(5))
            )
          )
        )
        _ <- undoable.undo
        _ <- undoable.get.map(v =>
          assert(
            v == Tree(
              Node(1, Node(2), Node(3)),
              Node(4, Node(5))
            )
          )
        )
        _ <- undoable.redo
        _ <- undoable.get.map(v =>
          assert(
            v == Tree(
              Node(1, Node(2), Node(3), Node(8)),
              Node(4, Node(5))
            )
          )
        )
      } yield ()).unsafeToFuture()
    }

    val vTreeMod = new TreeModByIdEq[IO, V, Int](_.id)

    test("TreeObjModPosUndoRedo") {
      (for {
        model <- Ref[IO].of(
          Tree(
            Node(V(1), Node(V(2)), Node(V(3))),
            Node(V(4), Node(V(5)))
          )
        )
        undoable <- TestUndoable(model)
        _        <- undoable.mod(vTreeMod.at(3), vTreeMod.setIdx((4.some, 1)))
        _ <- undoable.get.map(v =>
          assert(
            v == Tree(
              Node(V(1), Node(V(2))),
              Node(V(4), Node(V(5)), Node(V(3)))
            )
          )
        )
        _ <- model.update { t => // External modification, before undo
          val node1 = t.children.head
          val node2 = t.children.tail.head
          Tree(node1,
               Node(node2.value,
                    node2.children.init :+ Node(node2.children.last.value.copy(s = "three"))))
        }
        _ <- undoable.get.map(v =>
          assert(
            v == Tree(
              Node(V(1), Node(V(2))),
              Node(V(4), Node(V(5)), Node(V(3, "three")))
            )
          )
        )
        _ <- undoable.undo
        _ <- undoable.get.map(v =>
          assert(
            v == Tree(
              Node(V(1), Node(V(2)), Node(V(3, "three"))),
              Node(V(4), Node(V(5)))
            )
          )
        )
        _ <- undoable.redo
        _ <- undoable.get.map(v =>
          assert(
            v == Tree(
              Node(V(1), Node(V(2))),
              Node(V(4), Node(V(5)), Node(V(3, "three")))
            )
          )
        )
        _ <- undoable.undo
        _ <- model.update { t => // External modification, before redo
          val node1 = t.children.head
          val node2 = t.children.tail.head
          Tree(Node(node1.value,
                    node1.children.init :+ Node(node1.children.last.value.copy(s = "tres"))),
               node2)
        }
        _ <- undoable.get.map(v =>
          assert(
            v == Tree(
              Node(V(1), Node(V(2)), Node(V(3, "tres"))),
              Node(V(4), Node(V(5)))
            )
          )
        )
        _ <- undoable.redo
        _ <- undoable.get
          .map(v =>
            assert(
              v == Tree(
                Node(V(1), Node(V(2))),
                Node(V(4), Node(V(5)), Node(V(3, "tres")))
              )
            )
          )
      } yield ()).unsafeToFuture()
    }
  }
}
