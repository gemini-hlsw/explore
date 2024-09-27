// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.data.tree

import cats.kernel.Eq
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.numeric.NonNegShort
import explore.model.syntax.all.*

import scala.annotation.tailrec
import scala.collection.immutable.HashMap
import scala.collection.immutable.HashSet

import KeyedIndexedTree.*

case class KeyedIndexedTree[K: Eq, A] private (
  private val byKey:  Map[K, Node[IndexedElem[K, A]]],
  private val tree:   Tree[IndexedElem[K, A]]
)(
  private val getKey: A => K // Having in another param set keeps this out of equality
) {
  type Key = K

  def toTree: Tree[A] =
    tree.map(_.elem)

  def toKeyedTree: Tree[(K, A)] =
    tree.map(value => (getKey(value.elem), value.elem))

  def getNodeAndIndexByKey(key: K): Option[(Node[A], Index[K])] =
    byKey
      .get(key)
      .map: node =>
        (node.map(_.elem), node.value.index)

  def contains(key: K): Boolean =
    byKey.contains(key)

  def getKeyedNodeByIdx(index: Index[K]): Option[(K, Node[A])] =
    index.parentKey
      .fold(tree.children.some)(pkey => byKey.get(pkey).map(_.children))
      .flatMap:
        _.get(index.childPos.value.toLong).map(node => (getKey(node.value.elem), node.map(_.elem)))

  private def removeInTree(key: K): Tree[IndexedElem[K, A]] = {
    def removeInChildren(
      idx:       Index[K]
    )(
      children:  List[Node[IndexedElem[K, A]]],
      parentKey: Option[K] = None
    ): List[Node[IndexedElem[K, A]]] =
      if (parentKey === idx.parentKey)
        children.take(idx.childPos.value) ++ children
          .drop(idx.childPos.value + 1)
          .map:
            case Node(IndexedElem(e, Index(pkey, pos)), ch) =>
              Node(IndexedElem(e, Index(pkey, NonNegInt.unsafeFrom(pos.value - 1))), ch)
      else
        children.map: node =>
          Node(node.value, removeInChildren(idx)(node.children, getKey(node.value.elem).some))

    byKey
      .get(key)
      .fold(tree): keyElemIndex =>
        val idx: Index[K] = keyElemIndex.value.index
        Tree(removeInChildren(idx)(tree.children))
  }

  def removed(key: K): KeyedIndexedTree[K, A] =
    if (contains(key))
      val newTree: Tree[IndexedElem[K, A]] = removeInTree(key)
      KeyedIndexedTree(buildKeyMap(newTree, getKey), newTree)(getKey)
    else this

  def inserted(key: K, node: Node[A], idx: Index[K]): KeyedIndexedTree[K, A] = {
    def insertInChildren(
      children:  List[Node[IndexedElem[K, A]]],
      parentKey: Option[K] = None
    ): List[Node[IndexedElem[K, A]]] =
      if (parentKey === idx.parentKey)
        val fixedPos: NonNegInt = idx.childPos.value match
          case i if i > children.length => NonNegInt.unsafeFrom(children.length)
          case i                        => idx.childPos

        (children.take(fixedPos.value) :+
          Node(
            IndexedElem(node.value, Index(parentKey, fixedPos)),
            indexedUniqueKeyChildren(node.children, getKey, key.some, byKey.keySet + key)
          )) ++
          children
            .drop(fixedPos.value)
            .map:
              case Node(IndexedElem(e, Index(pkey, pos)), ch) =>
                Node(IndexedElem(e, Index(pkey, NonNegInt.unsafeFrom(pos.value + 1))), ch)
      else
        children.map: node =>
          Node(node.value, insertInChildren(node.children, getKey(node.value.elem).some))

    val cleanTree: Tree[IndexedElem[K, A]] = removeInTree(key)
    val newTree: Tree[IndexedElem[K, A]]   = Tree(insertInChildren(cleanTree.children))
    KeyedIndexedTree(buildKeyMap(newTree, getKey), newTree)(getKey)
  }

  def updated(key: K, newValue: A, newIndex: Index[K]): KeyedIndexedTree[K, A] =
    val existingChildren: List[Node[A]] = getNodeAndIndexByKey(key).map(_._1.children).orEmpty
    removed(key).inserted(key, Node(newValue, existingChildren), newIndex)

  def updated( // TODO TEST!!!
    key:             K,
    newValue:        A,
    parentKey:       Option[K],
    beforeNodeWhere: Node[A] => Boolean
  ): KeyedIndexedTree[K, A] =
    val newSiblings: Option[List[Node[A]]] =
      parentKey match
        case None       => tree.children.map(_.map(_.elem)).some
        case Some(pkey) => getNodeAndIndexByKey(pkey).map(_._1.children)
    // If newSiblings is None, then parentKey doesn't exist. We don't alter the tree.
    newSiblings.fold(this): siblings =>
      val newIndex: Int                           = Option(siblings.indexWhere(beforeNodeWhere))
        .filter(_ =!= -1)
        .getOrElse(siblings.length)
      val existingChildren: List[Node[A]]         = getNodeAndIndexByKey(key).map(_._1.children).orEmpty
      val withRemovedNode: KeyedIndexedTree[K, A] = removed(key)
      withRemovedNode.inserted(
        key,
        Node(newValue, existingChildren),
        Index(parentKey, NonNegInt.unsafeFrom(newIndex))
      )

  def collect[B](pf: PartialFunction[(K, Node[A], Index[K]), B]): List[B] =
    byKey
      .collect(pf.compose { case (k, node) => (k, node.map(_.elem), node.value.index) })
      .toList

  def parentKeys(key: K): List[K] = {
    @tailrec
    def go(
      acc:     List[K],
      current: K
    ): List[K] =
      byKey.get(current) match
        // Node not found, shouldn't happen 🤷
        case None                                                     => acc
        // We've found the 'root' node, so we're done
        case Some(Node(IndexedElem(_, Index(None, _)), _))            => acc
        // We've found a parent node, so we add it to the list and continue
        case Some(Node(IndexedElem(_, Index(Some(parentKey), _)), _)) =>
          go(parentKey :: acc, parentKey)
    go(Nil, key)
  }

  def mapElement[B](f: A => B, getKey: B => K): KeyedIndexedTree[K, B] = {
    val newTree: Tree[IndexedElem[K, B]] = tree.map { case IndexedElem(a, idx) =>
      IndexedElem(f(a), idx)
    }

    def mapNode(node: Node[IndexedElem[K, A]]): Node[IndexedElem[K, B]] =
      Node(IndexedElem(f(node.value.elem), node.value.index), node.children.map(mapNode))

    val newByKey = byKey.map((k, node) => (k, mapNode(node)))
    KeyedIndexedTree(newByKey, newTree)(getKey)
  }

}

object KeyedIndexedTree {
  case class Index[K](parentKey: Option[K], childPos: NonNegInt)
  object Index:
    inline def apply[K](parentKey: Option[K], childPos: NonNegShort): Index[K] =
      Index(parentKey, childPos.toNonNegInt)

  protected case class IndexedElem[K, A](elem: A, index: Index[K])

  protected def indexedUniqueKeyChildren[K, A](
    children:  List[Node[A]],
    getKey:    A => K,
    parentKey: Option[K] = None,
    accumKeys: Set[K] = HashSet.empty[K]
  ): List[Node[IndexedElem[K, A]]] = {
    val keyedNewNodes: List[(K, Node[A])] =
      children
        .map(node => (getKey(node.value), node))
        .distinctBy(_._1) // key
        .filterNot(accumKeys.contains.compose(_._1))
    val newAccumKeys: Set[K] = accumKeys ++ keyedNewNodes.map(_._1)
    keyedNewNodes.zipWithIndex.foldLeft(
      List.empty[Node[IndexedElem[K, A]]]
    ) { case (nodes, ((key, node), idx)) =>
      val newNodes = indexedUniqueKeyChildren(node.children, getKey, key.some, newAccumKeys)
      val newNode  =
        Node(IndexedElem(node.value, Index(parentKey, NonNegInt.unsafeFrom(idx))), newNodes)
      nodes :+ newNode
    }
  }

  protected def buildKeyMap[K, A](
    tree:   Tree[IndexedElem[K, A]],
    getKey: A => K
  ): Map[K, Node[IndexedElem[K, A]]] = {
    def childrenKeyMap(
      children: List[Node[IndexedElem[K, A]]]
    ): Map[K, Node[IndexedElem[K, A]]] =
      children.foldLeft(HashMap.empty[K, Node[IndexedElem[K, A]]]) { case (keyMap, node) =>
        keyMap + (getKey(node.value.elem) -> node) ++ childrenKeyMap(node.children)
      }

    childrenKeyMap(tree.children)
  }

  def fromTree[K: Eq, A](tree: Tree[A], getKey: A => K): KeyedIndexedTree[K, A] = {
    val indexedTree: Tree[IndexedElem[K, A]] = Tree(indexedUniqueKeyChildren(tree.children, getKey))
    KeyedIndexedTree(buildKeyMap(indexedTree, getKey), indexedTree)(getKey)
  }

  implicit def eqKeyedIndexedTree[K, A: Eq]: Eq[KeyedIndexedTree[K, A]] =
    Eq.by(_.toTree)
}
