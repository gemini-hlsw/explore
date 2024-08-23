// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.undo

import cats.effect.IO
import cats.effect.std.Dispatcher
import cats.kernel.Eq
import cats.syntax.all.*
import eu.timepit.refined.numeric.NonNegative
import eu.timepit.refined.types.numeric.NonNegInt
import explore.data.KeyedIndexedList
import explore.data.tree.*
import explore.data.tree.KeyedIndexedTree.Index
import explore.optics.Adjuster
import explore.optics.GetAdjust
import log4cats.loglevel.LogLevelLogger
import lucuma.refined.*
import monocle.Focus
import monocle.Iso
import monocle.Lens
import monocle.function.all.*
import monocle.macros.GenLens
import org.typelevel.log4cats.Logger

class UndoContextSpec extends munit.CatsEffectSuite {
  given Logger[IO] = LogLevelLogger.createForRoot[IO]

  def idLens[A] = Iso.id[A]

  def id[A] = GetAdjust(idLens[A])

  class ListModByIdEq[F[_], A, Id](idLens: Lens[A, Id]) extends KIListMod[A, Id](idLens)

  class ListModIdentityId[F[_], A] extends ListModByIdEq[F, A, A](idLens[A])

  val listIntMod = new ListModIdentityId[IO, Int]

  def kiList[K, A](a: A*)(getKey: A => K) = KeyedIndexedList.fromList(a.toList, getKey)

  def kiIntList(i: Int*) = kiList(i*)(idLens.get)

  val dispatcher = ResourceFunFixture(Dispatcher.parallel[IO])

  dispatcher.test("UndoRedo") { implicit dispatcher =>
    for {
      undoable <- TestUndoable[Int](0)
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
    } yield ()
  }

  dispatcher.test("ListModPosUndoRedo") { implicit dispatcher =>
    for {
      undoable <- TestUndoable[KeyedIndexedList[Int, Int]](kiIntList(1, 2, 3, 4, 5))
      _        <- undoable.mod(listIntMod.pos.withKey(3), listIntMod.pos.set(8.refined))
      _        <- undoable.get.map(v => assertEquals(v, kiIntList(1, 2, 4, 5, 3)))
      _        <- undoable.undo
      _        <- undoable.get.map(v => assertEquals(v, kiIntList(1, 2, 3, 4, 5)))
      _        <- undoable.redo
      _        <- undoable.get.map(v => assertEquals(v, kiIntList(1, 2, 4, 5, 3)))
    } yield ()
  }

  dispatcher.test("ListDeleteUndoRedo") { implicit dispatcher =>
    for {
      undoable <- TestUndoable[KeyedIndexedList[Int, Int]](kiIntList(1, 2, 3, 4, 5))
      _        <- undoable.mod(listIntMod.withKey(3), listIntMod.delete)
      _        <- undoable.get.map(v => assertEquals(v, kiIntList(1, 2, 4, 5)))
      _        <- undoable.undo
      _        <- undoable.get.map(v => assertEquals(v, kiIntList(1, 2, 3, 4, 5)))
      _        <- undoable.redo
      _        <- undoable.get.map(v => assertEquals(v, kiIntList(1, 2, 4, 5)))
    } yield ()
  }

  dispatcher.test("ListInsertUndoRedo") { implicit dispatcher =>
    for {
      undoable <- TestUndoable[KeyedIndexedList[Int, Int]](kiIntList(1, 2, 3, 4, 5))
      _        <- undoable.mod(listIntMod.withKey(8), listIntMod.upsert(8, 3.refined))
      _        <- undoable.get.map(v => assertEquals(v, kiIntList(1, 2, 3, 8, 4, 5)))
      _        <- undoable.undo
      _        <- undoable.get.map(v => assertEquals(v, kiIntList(1, 2, 3, 4, 5)))
      _        <- undoable.redo
      _        <- undoable.get.map(v => assertEquals(v, kiIntList(1, 2, 3, 8, 4, 5)))
    } yield ()
  }

  case class V(id: Int, s: String)
  object V {
    def apply(id: Int): V = V(id, id.toString)
    val id: Lens[V, Int]   = GenLens[V](_.id)
    val s: Lens[V, String] = GenLens[V](_.s)
  }

  val vListMod = new ListModByIdEq[IO, V, Int](V.id)

  def kiVList(v: V*) = kiList(v*)(V.id.get)

  def externalVListSetS(id: NonNegInt): Adjuster[KeyedIndexedList[Int, V], String] =
    vListMod
      .withKey(id.value)
      .adjuster
      .andThen(each[Option[(V, NonNegInt)], (V, NonNegInt)])
      .andThen(Focus[(V, NonNegInt)](_._1))
      .andThen(V.s)

  dispatcher.test("ListObjModPosUndoRedo") { implicit dispatcher =>
    for {
      undoable <- TestUndoable[KeyedIndexedList[Int, V]](kiVList(V(1), V(2), V(3), V(4), V(5)))
      _        <- undoable.mod(vListMod.pos.withKey(3), vListMod.pos.set(8.refined))
      _        <- undoable.get.map(v =>
                    assertEquals(v, kiVList(V(1, "1"), V(2, "2"), V(4, "4"), V(5, "5"), V(3, "3")))
                  )
      _        <- undoable.valueRef.update( // External modification, before undo
                    externalVListSetS(3.refined).set("three")
                  )
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
      _        <- undoable.valueRef.update( // External modification, before redo
                    externalVListSetS(3.refined).set("tres")
                  )
      _        <- undoable.get.map(v =>
                    assertEquals(v, kiVList(V(1, "1"), V(2, "2"), V(3, "tres"), V(4, "4"), V(5, "5")))
                  )
      _        <- undoable.redo
      _        <- undoable.get.map(v =>
                    assertEquals(v, kiVList(V(1, "1"), V(2, "2"), V(4, "4"), V(5, "5"), V(3, "tres")))
                  )
    } yield ()
  }

  class TreeModByIdEq[F[_], A, K](keyLens: Lens[A, K]) extends KITreeMod[A, K](keyLens)

  class TreeModIdentityId[F[_], A] extends TreeModByIdEq[F, A, A](idLens[A])

  val treeIntMod = new TreeModIdentityId[IO, Int]

  def kiTree[K: Eq, A](tree: Tree[A])(getKey: A => K) = KeyedIndexedTree.fromTree(tree, getKey)

  def kiIntTree(tree: Tree[Int]) = kiTree(tree)(idLens.get)

  dispatcher.test("TreeModPosUndoRedo") { implicit dispatcher =>
    for {
      undoable <- TestUndoable[KeyedIndexedTree[Int, Int]](
                    kiIntTree(
                      Tree(
                        Node(1, Node(2), Node(3)),
                        Node(4, Node(5))
                      )
                    )
                  )
      _        <- undoable.mod(treeIntMod.pos.withKey(3),
                               treeIntMod.pos.set(Index(4.some, 1.refined[NonNegative]))
                  )
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
    } yield ()
  }

  dispatcher.test("TreeDeleteUndoRedo") { implicit dispatcher =>
    for {
      undoable <- TestUndoable[KeyedIndexedTree[Int, Int]](
                    kiIntTree(
                      Tree(
                        Node(1, Node(2), Node(3)),
                        Node(4, Node(5))
                      )
                    )
                  )
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
    } yield ()
  }

  dispatcher.test("TreeInsertUndoRedo") { implicit dispatcher =>
    for {
      undoable <- TestUndoable[KeyedIndexedTree[Int, Int]](
                    kiIntTree(
                      Tree(
                        Node(1, Node(2), Node(3)),
                        Node(4, Node(5))
                      )
                    )
                  )
      _        <- undoable.mod(treeIntMod.withKey(8),
                               treeIntMod.upsert(8, Index(1.some, 8.refined[NonNegative]))
                  )
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
    } yield ()
  }

  val vTreeMod = new TreeModByIdEq[IO, V, Int](V.id)

  def kiVTree(tree: Tree[V]) = kiTree(tree)(V.id.get)

  def externalVTreeSetS(key: Int): Adjuster[KeyedIndexedTree[Int, V], String] =
    vTreeMod
      .withKey(key)
      .adjuster
      .andThen(each[Option[(Node[V], Index[Int])], (Node[V], Index[Int])])
      .andThen(Focus[(Node[V], Index[Int])](_._1))
      .andThen(Node.value[V])
      .andThen(V.s)

  dispatcher.test("TreeObjModPosUndoRedo") { implicit dispatcher =>
    for {
      undoable <- TestUndoable[KeyedIndexedTree[Int, V]](
                    kiVTree(
                      Tree(
                        Node(V(1), Node(V(2)), Node(V(3))),
                        Node(V(4), Node(V(5)))
                      )
                    )
                  )
      _        <- undoable.mod(vTreeMod.pos.withKey(3),
                               vTreeMod.pos.set(Index(4.some, 1.refined[NonNegative]))
                  )
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
      _        <- undoable.valueRef.update( // External modification, before undo
                    externalVTreeSetS(3).set("three")
                  )
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
      _        <- undoable.valueRef.update( // External modification, before undo
                    externalVTreeSetS(3).set("tres")
                  )
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
    } yield ()
  }
}
