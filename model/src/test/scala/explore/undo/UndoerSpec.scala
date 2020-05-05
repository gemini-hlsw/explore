// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.undo

import cats.effect.IO
import cats.implicits._
import cats.effect.concurrent.Ref
import monocle.Iso
import cats.kernel.Eq
import explore.undo._
import explore.util.tree._
import monocle.macros.Lenses
import monocle.function.all._
import monocle.Lens
import monocle.Setter

class UndoerSpec extends munit.FunSuite {

  def idLens[A] = Iso.id[A].asLens

  def id[A] = GetSet(idLens[A])

  class ListModByIdEq[F[_], A, Id: Eq](idLens: Lens[A, Id]) extends ListMod[F, A, Id](idLens)

  class ListModIdentityId[F[_], A: Eq] extends ListModByIdEq[F, A, A](idLens[A])

  val listIntMod = new ListModIdentityId[IO, Int]

  test("UndoRedo") {
    (for {
      model    <- Ref[IO].of(0)
      undoable <- TestUndoable(model)
      _        <- undoable.set(id[Int], 1)
      _        <- undoable.set(id[Int], 2)
      _        <- undoable.set(id[Int], 3)
      _        <- undoable.get.map(v => assertEquals(v, 3))
      _        <- undoable.undo
      _        <- undoable.get.map(v => assertEquals(v, 2))
      _        <- undoable.undo
      _        <- undoable.get.map(v => assertEquals(v, 1))
      _        <- undoable.redo
      _        <- undoable.get.map(v => assertEquals(v, 2))
      _        <- undoable.redo
      _        <- undoable.get.map(v => assertEquals(v, 3))
    } yield ()).unsafeToFuture()
  }

  test("ListModPosUndoRedo") {
    (for {
      model    <- Ref[IO].of(List(1, 2, 3, 4, 5))
      undoable <- TestUndoable(model)
      _        <- undoable.mod(listIntMod.pos.withId(3), listIntMod.pos.set(8))
      _        <- undoable.get.map(v => assertEquals(v, List(1, 2, 4, 5, 3)))
      _        <- undoable.undo
      _        <- undoable.get.map(v => assertEquals(v, List(1, 2, 3, 4, 5)))
      _        <- undoable.redo
      _        <- undoable.get.map(v => assertEquals(v, List(1, 2, 4, 5, 3)))
    } yield ()).unsafeToFuture()
  }

  test("ListDeleteUndoRedo") {
    (for {
      model    <- Ref[IO].of(List(1, 2, 3, 4, 5))
      undoable <- TestUndoable(model)
      _        <- undoable.mod(listIntMod.withId(3), listIntMod.delete)
      _        <- undoable.get.map(v => assertEquals(v, List(1, 2, 4, 5)))
      _        <- undoable.undo
      _        <- undoable.get.map(v => assertEquals(v, List(1, 2, 3, 4, 5)))
      _        <- undoable.redo
      _        <- undoable.get.map(v => assertEquals(v, List(1, 2, 4, 5)))
    } yield ()).unsafeToFuture()
  }

  test("ListInsertUndoRedo") {
    (for {
      model    <- Ref[IO].of(List(1, 2, 3, 4, 5))
      undoable <- TestUndoable(model)
      _        <- undoable.mod(listIntMod.withId(8), listIntMod.upsert(8, 3))
      _        <- undoable.get.map(v => assertEquals(v, List(1, 2, 3, 8, 4, 5)))
      _        <- undoable.undo
      _        <- undoable.get.map(v => assertEquals(v, List(1, 2, 3, 4, 5)))
      _        <- undoable.redo
      _        <- undoable.get.map(v => assertEquals(v, List(1, 2, 3, 8, 4, 5)))
    } yield ()).unsafeToFuture()
  }

  @Lenses
  case class V(id: Int, s: String)
  object V {
    def apply(id: Int): V = V(id, id.toString)
  }

  val vListMod = new ListModByIdEq[IO, V, Int](V.id)

  def externalVListSetS(id: Int): Setter[List[V], String] =
    vListMod
      .withId(id)
      .setter
      .composeTraversal(each)
      .composeLens(first)
      .composeLens(V.s)

  test("ListObjModPosUndoRedo") {
    (for {
      model    <- Ref[IO].of(List(V(1), V(2), V(3), V(4), V(5)))
      undoable <- TestUndoable(model)
      _        <- undoable.mod(vListMod.pos.withId(3), vListMod.pos.set(8))
      _        <- undoable.get.map(v =>
             assertEquals(v, List(V(1, "1"), V(2, "2"), V(4, "4"), V(5, "5"), V(3, "3")))
           )
      _        <- model.update(externalVListSetS(3).set("three")) // External modification, before undo
      _        <- undoable.get.map(v =>
             assertEquals(v, List(V(1, "1"), V(2, "2"), V(4, "4"), V(5, "5"), V(3, "three")))
           )
      _        <- undoable.undo
      _        <- undoable.get.map(v =>
             assertEquals(v, List(V(1, "1"), V(2, "2"), V(3, "three"), V(4, "4"), V(5, "5")))
           )
      _        <- undoable.redo
      _        <- undoable.get.map(v =>
             assertEquals(v, List(V(1, "1"), V(2, "2"), V(4, "4"), V(5, "5"), V(3, "three")))
           )
      _        <- undoable.undo
      _        <- model.update(externalVListSetS(3).set("tres")) // External modification, before redo
      _        <- undoable.get.map(v =>
             assertEquals(v, List(V(1, "1"), V(2, "2"), V(3, "tres"), V(4, "4"), V(5, "5")))
           )
      _        <- undoable.redo
      _        <- undoable.get.map(v =>
             assertEquals(v, List(V(1, "1"), V(2, "2"), V(4, "4"), V(5, "5"), V(3, "tres")))
           )
    } yield ()).unsafeToFuture()
  }

  class TreeModByIdEq[F[_], A, Id: Eq](idLens: Lens[A, Id]) extends TreeMod[F, A, Id](idLens)

  class TreeModIdentityId[F[_], A: Eq] extends TreeModByIdEq[F, A, A](idLens[A])

  val treeIntMod = new TreeModIdentityId[IO, Int]

  test("TreeModPosUndoRedo") {
    (for {
      model    <- Ref[IO].of(
                 Tree(
                   Node(1, Node(2), Node(3)),
                   Node(4, Node(5))
                 )
               )
      undoable <- TestUndoable(model)
      _        <- undoable.mod(treeIntMod.pos.withId(3), treeIntMod.pos.set((4.some, 1)))
      _        <- undoable.get.map(v =>
             assert(
               v ==
                 Tree(
                   Node(1, Node(2)),
                   Node(4, Node(5), Node(3))
                 )
             )
           )
      _        <- undoable.undo
      _        <- undoable.get.map(v =>
             assert(
               v ==
                 Tree(
                   Node(1, Node(2), Node(3)),
                   Node(4, Node(5))
                 )
             )
           )
      _        <- undoable.redo
      _        <- undoable.get.map(v =>
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
      model    <- Ref[IO].of(
                 Tree(
                   Node(1, Node(2), Node(3)),
                   Node(4, Node(5))
                 )
               )
      undoable <- TestUndoable(model)
      _        <- undoable.mod(treeIntMod.withId(3), treeIntMod.delete)
      _        <- undoable.get.map(v =>
             assert(
               v ==
                 Tree(
                   Node(1, Node(2)),
                   Node(4, Node(5))
                 )
             )
           )
      _        <- undoable.undo
      _        <- undoable.get.map(v =>
             assert(
               v == Tree(
                 Node(1, Node(2), Node(3)),
                 Node(4, Node(5))
               )
             )
           )
      _        <- undoable.redo
      _        <- undoable.get.map(v =>
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
      model    <- Ref[IO].of(
                 Tree(
                   Node(1, Node(2), Node(3)),
                   Node(4, Node(5))
                 )
               )
      undoable <- TestUndoable(model)
      _        <- undoable.mod(treeIntMod.withId(8), treeIntMod.upsert(8, (1.some, 8)))
      _        <- undoable.get.map(v =>
             assert(
               v == Tree(
                 Node(1, Node(2), Node(3), Node(8)),
                 Node(4, Node(5))
               )
             )
           )
      _        <- undoable.undo
      _        <- undoable.get.map(v =>
             assert(
               v == Tree(
                 Node(1, Node(2), Node(3)),
                 Node(4, Node(5))
               )
             )
           )
      _        <- undoable.redo
      _        <- undoable.get.map(v =>
             assert(
               v == Tree(
                 Node(1, Node(2), Node(3), Node(8)),
                 Node(4, Node(5))
               )
             )
           )
    } yield ()).unsafeToFuture()
  }

  val vTreeMod = new TreeModByIdEq[IO, V, Int](V.id)

  def externalVTreeSetS(id: Int): Setter[Tree[V], String] =
    vTreeMod
      .withId(id)
      .setter
      .composeTraversal(each)
      .composeLens(first)
      .composeLens(V.s)

  test("TreeObjModPosUndoRedo") {
    (for {
      model    <- Ref[IO].of(
                 Tree(
                   Node(V(1), Node(V(2)), Node(V(3))),
                   Node(V(4), Node(V(5)))
                 )
               )
      undoable <- TestUndoable(model)
      _        <- undoable.mod(vTreeMod.pos.withId(3), vTreeMod.pos.set((4.some, 1)))
      _        <- undoable.get.map(v =>
             assert(
               v == Tree(
                 Node(V(1), Node(V(2))),
                 Node(V(4), Node(V(5)), Node(V(3)))
               )
             )
           )
      _        <- model.update(externalVTreeSetS(3).set("three")) // External modification, before undo
      _        <- undoable.get.map(v =>
             assert(
               v == Tree(
                 Node(V(1), Node(V(2))),
                 Node(V(4), Node(V(5)), Node(V(3, "three")))
               )
             )
           )
      _        <- undoable.undo
      _        <- undoable.get.map(v =>
             assert(
               v == Tree(
                 Node(V(1), Node(V(2)), Node(V(3, "three"))),
                 Node(V(4), Node(V(5)))
               )
             )
           )
      _        <- undoable.redo
      _        <- undoable.get.map(v =>
             assert(
               v == Tree(
                 Node(V(1), Node(V(2))),
                 Node(V(4), Node(V(5)), Node(V(3, "three")))
               )
             )
           )
      _        <- undoable.undo
      _        <- model.update(externalVTreeSetS(3).set("tres")) // External modification, before undo
      _        <- undoable.get.map(v =>
             assert(
               v == Tree(
                 Node(V(1), Node(V(2)), Node(V(3, "tres"))),
                 Node(V(4), Node(V(5)))
               )
             )
           )
      _        <- undoable.redo
      _        <- undoable.get
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
