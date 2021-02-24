// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.undo

import cats.effect.IO
import cats.effect.concurrent.Ref
import cats.syntax.all._
import cats.kernel.Eq
import explore.data.KeyedIndexedList
import explore.data.tree.KeyedIndexedTree.Index
import explore.data.tree._
import explore.optics.Adjuster
import explore.optics.GetAdjust
import explore.undo._
import monocle.Lens
import monocle.function.all._
import monocle.macros.GenLens

class UndoerSpec extends munit.FunSuite {

  def idLens[A] = Lens.id[A]

  def id[A] = GetAdjust(idLens[A])

  class ListModByIdEq[F[_], A, Id](idLens: Lens[A, Id]) extends KIListMod[F, A, Id](idLens)

  class ListModIdentityId[F[_], A] extends ListModByIdEq[F, A, A](idLens[A])

  val listIntMod = new ListModIdentityId[IO, Int]

  def kiList[K, A](a: A*)(getKey: A => K) = KeyedIndexedList.fromList(a.toList, getKey)

  def kiIntList(i: Int*) = kiList(i: _*)(idLens.get)

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
      model    <- Ref[IO].of(kiIntList(1, 2, 3, 4, 5))
      undoable <- TestUndoable(model)
      _        <- undoable.mod(listIntMod.pos.withKey(3), listIntMod.pos.set(8))
      _        <- undoable.get.map(v => assertEquals(v, kiIntList(1, 2, 4, 5, 3)))
      _        <- undoable.undo
      _        <- undoable.get.map(v => assertEquals(v, kiIntList(1, 2, 3, 4, 5)))
      _        <- undoable.redo
      _        <- undoable.get.map(v => assertEquals(v, kiIntList(1, 2, 4, 5, 3)))
    } yield ()).unsafeToFuture()
  }

  test("ListDeleteUndoRedo") {
    (for {
      model    <- Ref[IO].of(kiIntList(1, 2, 3, 4, 5))
      undoable <- TestUndoable(model)
      _        <- undoable.mod(listIntMod.withKey(3), listIntMod.delete)
      _        <- undoable.get.map(v => assertEquals(v, kiIntList(1, 2, 4, 5)))
      _        <- undoable.undo
      _        <- undoable.get.map(v => assertEquals(v, kiIntList(1, 2, 3, 4, 5)))
      _        <- undoable.redo
      _        <- undoable.get.map(v => assertEquals(v, kiIntList(1, 2, 4, 5)))
    } yield ()).unsafeToFuture()
  }

  test("ListInsertUndoRedo") {
    (for {
      model    <- Ref[IO].of(kiIntList(1, 2, 3, 4, 5))
      undoable <- TestUndoable(model)
      _        <- undoable.mod(listIntMod.withKey(8), listIntMod.upsert(8, 3))
      _        <- undoable.get.map(v => assertEquals(v, kiIntList(1, 2, 3, 8, 4, 5)))
      _        <- undoable.undo
      _        <- undoable.get.map(v => assertEquals(v, kiIntList(1, 2, 3, 4, 5)))
      _        <- undoable.redo
      _        <- undoable.get.map(v => assertEquals(v, kiIntList(1, 2, 3, 8, 4, 5)))
    } yield ()).unsafeToFuture()
  }

  // @Lenses
  case class V(id: Int, s: String)
  object V {
    def apply(id: Int): V = V(id, id.toString)
    // @Lenses doesn't seem to be working for some reason...
    val id: Lens[V, Int]   = GenLens[V](_.id)
    val s: Lens[V, String] = GenLens[V](_.s)
  }

  val vListMod = new ListModByIdEq[IO, V, Int](V.id)

  def kiVList(v: V*) = kiList(v: _*)(V.id.get)

  def externalVListSetS(id: Int): Adjuster[KeyedIndexedList[Int, V], String] =
    vListMod
      .withKey(id)
      .adjuster
      .composeTraversal(each)
      .composeLens(first)
      .composeLens(V.s)

  test("ListObjModPosUndoRedo") {
    (for {
      model    <- Ref[IO].of(kiVList(V(1), V(2), V(3), V(4), V(5)))
      undoable <- TestUndoable(model)
      _        <- undoable.mod(vListMod.pos.withKey(3), vListMod.pos.set(8))
      _        <- undoable.get.map(v =>
                    assertEquals(v, kiVList(V(1, "1"), V(2, "2"), V(4, "4"), V(5, "5"), V(3, "3")))
                  )
      _        <- model.update(externalVListSetS(3).set("three")) // External modification, before undo
      _        <- undoable.get.map(v =>
                    assertEquals(v, kiVList(V(1, "1"), V(2, "2"), V(4, "4"), V(5, "5"), V(3, "three")))
                  )
      _        <- undoable.undo
      _        <- undoable.get.map(v =>
                    assertEquals(v, kiVList(V(1, "1"), V(2, "2"), V(3, "three"), V(4, "4"), V(5, "5")))
                  )
      _        <- undoable.redo
      _        <- undoable.get.map(v =>
                    assertEquals(v, kiVList(V(1, "1"), V(2, "2"), V(4, "4"), V(5, "5"), V(3, "three")))
                  )
      _        <- undoable.undo
      _        <- model.update(externalVListSetS(3).set("tres")) // External modification, before redo
      _        <- undoable.get.map(v =>
                    assertEquals(v, kiVList(V(1, "1"), V(2, "2"), V(3, "tres"), V(4, "4"), V(5, "5")))
                  )
      _        <- undoable.redo
      _        <- undoable.get.map(v =>
                    assertEquals(v, kiVList(V(1, "1"), V(2, "2"), V(4, "4"), V(5, "5"), V(3, "tres")))
                  )
    } yield ()).unsafeToFuture()
  }

  class TreeModByIdEq[F[_], A, K](keyLens: Lens[A, K]) extends KITreeMod[F, A, K](keyLens)

  class TreeModIdentityId[F[_], A] extends TreeModByIdEq[F, A, A](idLens[A])

  val treeIntMod = new TreeModIdentityId[IO, Int]

  def kiTree[K: Eq, A](tree: Tree[A])(getKey: A => K) = KeyedIndexedTree.fromTree(tree, getKey)

  def kiIntTree(tree: Tree[Int]) = kiTree(tree)(idLens.get)

  test("TreeModPosUndoRedo") {
    (for {
      model    <- Ref[IO].of(
                    kiIntTree(
                      Tree(
                        Node(1, Node(2), Node(3)),
                        Node(4, Node(5))
                      )
                    )
                  )
      undoable <- TestUndoable(model)
      _        <- undoable.mod(treeIntMod.pos.withKey(3), treeIntMod.pos.set(Index(4.some, 1)))
      _        <- undoable.get.map(v =>
                    assertEquals(
                      v,
                      kiIntTree(
                        Tree(
                          Node(1, Node(2)),
                          Node(4, Node(5), Node(3))
                        )
                      )
                    )
                  )
      _        <- undoable.undo
      _        <- undoable.get.map(v =>
                    assertEquals(
                      v,
                      kiIntTree(
                        Tree(
                          Node(1, Node(2), Node(3)),
                          Node(4, Node(5))
                        )
                      )
                    )
                  )
      _        <- undoable.redo
      _        <- undoable.get.map(v =>
                    assertEquals(v,
                                 kiIntTree(
                                   Tree(
                                     Node(1, Node(2)),
                                     Node(4, Node(5), Node(3))
                                   )
                                 )
                    )
                  )
    } yield ()).unsafeToFuture()
  }

  test("TreeDeleteUndoRedo") {
    (for {
      model    <- Ref[IO].of(
                    kiIntTree(
                      Tree(
                        Node(1, Node(2), Node(3)),
                        Node(4, Node(5))
                      )
                    )
                  )
      undoable <- TestUndoable(model)
      _        <- undoable.mod(treeIntMod.withKey(3), treeIntMod.delete)
      _        <- undoable.get.map(v =>
                    assertEquals(v,
                                 kiIntTree(
                                   Tree(
                                     Node(1, Node(2)),
                                     Node(4, Node(5))
                                   )
                                 )
                    )
                  )
      _        <- undoable.undo
      _        <- undoable.get.map(v =>
                    assertEquals(v,
                                 kiIntTree(
                                   Tree(
                                     Node(1, Node(2), Node(3)),
                                     Node(4, Node(5))
                                   )
                                 )
                    )
                  )
      _        <- undoable.redo
      _        <- undoable.get.map(v =>
                    assertEquals(v,
                                 kiIntTree(
                                   Tree(
                                     Node(1, Node(2)),
                                     Node(4, Node(5))
                                   )
                                 )
                    )
                  )
    } yield ()).unsafeToFuture()
  }

  test("TreeInsertUndoRedo") {
    (for {
      model    <- Ref[IO].of(
                    kiIntTree(
                      Tree(
                        Node(1, Node(2), Node(3)),
                        Node(4, Node(5))
                      )
                    )
                  )
      undoable <- TestUndoable(model)
      _        <- undoable.mod(treeIntMod.withKey(8), treeIntMod.upsert(8, Index(1.some, 8)))
      _        <- undoable.get.map(v =>
                    assertEquals(v,
                                 kiIntTree(
                                   Tree(
                                     Node(1, Node(2), Node(3), Node(8)),
                                     Node(4, Node(5))
                                   )
                                 )
                    )
                  )
      _        <- undoable.undo
      _        <- undoable.get.map(v =>
                    assertEquals(v,
                                 kiIntTree(
                                   Tree(
                                     Node(1, Node(2), Node(3)),
                                     Node(4, Node(5))
                                   )
                                 )
                    )
                  )
      _        <- undoable.redo
      _        <- undoable.get.map(v =>
                    assertEquals(v,
                                 kiIntTree(
                                   Tree(
                                     Node(1, Node(2), Node(3), Node(8)),
                                     Node(4, Node(5))
                                   )
                                 )
                    )
                  )
    } yield ()).unsafeToFuture()
  }

  val vTreeMod = new TreeModByIdEq[IO, V, Int](V.id)

  def kiVTree(tree: Tree[V]) = kiTree(tree)(V.id.get)

  def externalVTreeSetS(key: Int): Adjuster[KeyedIndexedTree[Int, V], String] =
    vTreeMod
      .withKey(key)
      .adjuster
      .composeTraversal(each)
      .composeLens(first)
      .composeLens(Node.value)
      .composeLens(V.s)

  test("TreeObjModPosUndoRedo") {
    (for {
      model    <- Ref[IO].of(
                    kiVTree(
                      Tree(
                        Node(V(1), Node(V(2)), Node(V(3))),
                        Node(V(4), Node(V(5)))
                      )
                    )
                  )
      undoable <- TestUndoable(model)
      _        <- undoable.mod(vTreeMod.pos.withKey(3), vTreeMod.pos.set(Index(4.some, 1)))
      _        <- undoable.get.map(v =>
                    assertEquals(v,
                                 kiVTree(
                                   Tree(
                                     Node(V(1), Node(V(2))),
                                     Node(V(4), Node(V(5)), Node(V(3)))
                                   )
                                 )
                    )
                  )
      _        <- model.update(externalVTreeSetS(3).set("three")) // External modification, before undo
      _        <- undoable.get.map(v =>
                    assertEquals(v,
                                 kiVTree(
                                   Tree(
                                     Node(V(1), Node(V(2))),
                                     Node(V(4), Node(V(5)), Node(V(3, "three")))
                                   )
                                 )
                    )
                  )
      _        <- undoable.undo
      _        <- undoable.get.map(v =>
                    assertEquals(v,
                                 kiVTree(
                                   Tree(
                                     Node(V(1), Node(V(2)), Node(V(3, "three"))),
                                     Node(V(4), Node(V(5)))
                                   )
                                 )
                    )
                  )
      _        <- undoable.redo
      _        <- undoable.get.map(v =>
                    assertEquals(v,
                                 kiVTree(
                                   Tree(
                                     Node(V(1), Node(V(2))),
                                     Node(V(4), Node(V(5)), Node(V(3, "three")))
                                   )
                                 )
                    )
                  )
      _        <- undoable.undo
      _        <- model.update(externalVTreeSetS(3).set("tres")) // External modification, before undo
      _        <- undoable.get.map(v =>
                    assertEquals(v,
                                 kiVTree(
                                   Tree(
                                     Node(V(1), Node(V(2)), Node(V(3, "tres"))),
                                     Node(V(4), Node(V(5)))
                                   )
                                 )
                    )
                  )
      _        <- undoable.redo
      _        <- undoable.get
                    .map(v =>
                      assertEquals(v,
                                   kiVTree(
                                     Tree(
                                       Node(V(1), Node(V(2))),
                                       Node(V(4), Node(V(5)), Node(V(3, "tres")))
                                     )
                                   )
                      )
                    )
    } yield ()).unsafeToFuture()
  }
}
