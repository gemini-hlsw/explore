// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.Order.*
import cats.syntax.all.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.numeric.NonNegShort
import explore.data.tree.KeyedIndexedTree
import explore.data.tree.Node as TreeNode
import explore.data.tree.Tree
import explore.model.syntax.all.*
import monocle.Lens

type GroupTree = KeyedIndexedTree[GroupTree.Key, GroupTree.Value]

object GroupTree:
  type Key   = Either[Observation.Id, Group.Id]
  type Index = KeyedIndexedTree.Index[Key]
  type Value = ServerIndexed[Either[Observation.Id, Group]]
  type Node  = TreeNode[Value]

  object syntax:
    extension (self: ServerIndexed[Either[Observation.Id, Group]])
      def id: Either[Observation.Id, Group.Id] = self.elem.map(_.id)

    extension (self: GroupTree)
      def obsGroupId(obsId: Observation.Id): Option[Group.Id] =
        self.getNodeAndIndexByKey(Left(obsId)).flatMap(_._2.parentKey.flatMap(_.toOption))

  import syntax.*

  def key: Lens[Value, Key] = Lens[Value, Key](_.id)(newId =>
    case old @ ServerIndexed(Left(oldObsId), parentIndex)  =>
      newId match
        case Left(newObsId) => ServerIndexed(newObsId.asLeft, parentIndex)
        case _              => old
    case old @ ServerIndexed(Right(oldGroup), parentIndex) =>
      newId match
        case Right(newGroupId) =>
          ServerIndexed(Group.id.replace(newGroupId)(oldGroup).asRight, parentIndex)
        case _                 => old
  )

  def fromList(groups: List[GroupElement]): GroupTree = {
    // For faster lookup when creating the tree
    val groupMap: Map[Group.Id, Grouping] =
      groups.mapFilter(_.value.toOption.map(g => g.group.id -> g)).toMap

    def createObsNode(obsId: Observation.Id, parentIndex: NonNegShort): Node =
      TreeNode(ServerIndexed(obsId.asLeft, parentIndex), Nil)

    def createGroupNode(groupId: Group.Id, parentIndex: NonNegShort): Node =
      val grouping: Grouping   = groupMap(groupId)
      val children: List[Node] =
        grouping.elements
          .map(child => toChild(child.elem, child.parentIndex))
          .sortBy(_.value.parentIndex)
      TreeNode(ServerIndexed(grouping.group.asRight, parentIndex), children)

    def toChild(elem: Either[Observation.Id, Group.Id], parentIndex: NonNegShort): Node =
      elem.fold(createObsNode(_, parentIndex), createGroupNode(_, parentIndex))

    val rootElems: List[Node] =
      groups
        .mapFilter: child =>
          child.indexInRootGroup
            .map(idx => toChild(child.value.map(_.group.id), idx))
        .sortBy(_.value.parentIndex)

    KeyedIndexedTree.fromTree(Tree(rootElems), _.id)
  }

// parentIndices may skip values, since they also index deleted elements, so we have to keep track of them.
case class ServerIndexed[A](elem: A, parentIndex: NonNegShort):
  def map[B](f: A => B): ServerIndexed[B] = ServerIndexed(f(elem), parentIndex)

object ServerIndexed:
  given [A: Eq]: Eq[ServerIndexed[A]] = Eq.by(x => (x.elem, x.parentIndex))
