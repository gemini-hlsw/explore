// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.Order.*
import cats.derived.*
import cats.syntax.all.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.numeric.NonNegShort
import eu.timepit.refined.types.string.NonEmptyString
import explore.data.tree.KeyedIndexedTree
import explore.data.tree.Node as TreeNode
import explore.data.tree.Tree
import explore.model.syntax.all.*
import lucuma.core.util.TimeSpan
import monocle.Focus
import monocle.Lens

type GroupTree = KeyedIndexedTree[GroupTree.Key, GroupTree.Value]

object GroupTree:
  type Key   = Either[Observation.Id, Group.Id]
  type Index = KeyedIndexedTree.Index[Key]
  type Value = ServerIndexed[Either[Observation.Id, Group]]
  type Node  = TreeNode[Value]

  // def key: Lens[Value, Key] = Lens[Value, Key](_.bimap(_.id, _.id))(newId =>
  //   case Left(obs)    =>
  //     newId match
  //       case Left(id) => Obs.id.replace(id)(obs).asLeft
  //       case _        => obs.asLeft
  //   case Right(group) =>
  //     newId match
  //       case Right(id) => Group.id.replace(id)(group).asRight
  //       case _         => group.asRight
  // )

  def fromList(groups: List[GroupElement]): GroupTree = {
    // For faster lookup when creating the tree
    val (obsList, groupList): (List[(Observation.Id, ServerIndexed[Observation.Id])],
                               List[(Group.Id, ServerIndexed[Grouping])]
    ) =
      groups.partitionMap:
        _.value match
          case ServerIndexed(Left(obsId), parentIndex)     =>
            Left(obsId -> ServerIndexed(obsId, parentIndex))
          case ServerIndexed(Right(grouping), parentIndex) =>
            Right(grouping.group.id -> ServerIndexed(grouping, parentIndex))

    val obsMap: Map[Observation.Id, ServerIndexed[Observation.Id]] = obsList.toMap
    val groupMap: Map[Group.Id, ServerIndexed[Grouping]]           = groupList.toMap

    def createObsNode(obsId: Observation.Id): Node =
      TreeNode(obsMap(obsId).map(_.asLeft), Nil)

    def createGroupNode(groupId: Group.Id): Node =
      val grouping: ServerIndexed[Grouping] = groupMap(groupId)
      val children: List[Node]              = toChildren(grouping.elem.elements)
      TreeNode(grouping.map(_.group.asRight), children)

    def toChildren(elems: List[Either[Observation.Id, Group.Id]]): List[Node] =
      elems.map(_.fold(createObsNode, createGroupNode)).sortBy(_.value.parentIndex)

    val rootElems: List[Either[Observation.Id, Group.Id]] =
      groups
        .mapFilter(g => Option.when(g.parentGroupId.isEmpty)(g.value.elem.map(_.group.id)))

    KeyedIndexedTree.fromTree(Tree(toChildren(rootElems)), _.elem.map(_.id))
  }

// parentIndices may skip values, since they also index deleted elements, so we have to keep track of them.
case class ServerIndexed[A](elem: A, parentIndex: NonNegShort):
  def map[B](f: A => B): ServerIndexed[B] = ServerIndexed(f(elem), parentIndex)
