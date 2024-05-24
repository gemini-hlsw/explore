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
import lucuma.core.model.Group.Id as GroupId
import lucuma.core.model.Observation
import lucuma.core.util.TimeSpan
import monocle.Focus
import monocle.Lens

type GroupTree = KeyedIndexedTree[GroupTree.Key, GroupTree.Value]

object GroupTree:

  type Key   = Either[Observation.Id, GroupId]
  type Index = KeyedIndexedTree.Index[Key]
  type Value = Either[Obs, Group]
  type Node  = TreeNode[Value]

  def key: Lens[Value, Key] = Lens[Value, Key](_.bimap(_.id, _.id))(newId =>
    case Left(obs)    =>
      newId match
        case Left(id) => Obs.id.replace(id)(obs).asLeft
        case _        => obs.asLeft
    case Right(group) =>
      newId match
        case Right(id) => Group.id.replace(id)(group).asRight
        case _         => group.asRight
  )

  def fromList(groups: List[GroupElement]): GroupTree = {
    // For faster lookup when creating the tree
    val (obsList, groupList) = groups.partitionMap(_.value)
    val obsMap               = obsList.map(obs => (obs.id, obs)).toMap
    val groupMap             = groupList.map(group => (group.id, group)).toMap

    def createObsNode(obs: GroupObs): Node = TreeNode(Obs(obs.id).asLeft[Group], Nil)

    def createGroupNode(group: Grouping): Node =
      val children = group.elements
        .flatMap(
          _.fold(
            obs => obsMap.get(obs.id).map(_.asLeft),
            group => groupMap.get(group.id).map(_.asRight)
          )
        )
        .sortBy(_.groupIndex)
        .map(_.fold(createObsNode(_), createGroupNode))
      TreeNode(group.toGroupTreeGroup.asRight, children)

    val rootGroups =
      groups
        .mapFilter(g => if g.parentGroupId.isEmpty then g.value.some else none)
        .sortBy(_.groupIndex)

    val nodes = rootGroups.map(_.fold(createObsNode, createGroupNode))

    KeyedIndexedTree.fromTree(Tree(nodes), _.id)
  }

  case class Group(
    id:              GroupId,
    name:            Option[NonEmptyString],
    minimumRequired: Option[NonNegShort],
    minimumInterval: Option[TimeSpan],
    maximumInterval: Option[TimeSpan],
    ordered:         Boolean
  ) derives Eq:
    def isAnd: Boolean = minimumRequired.isEmpty

  object Group:
    val id: Lens[Group, GroupId] = Focus[Group](_.id)

    val name: Lens[Group, Option[NonEmptyString]] = Focus[Group](_.name)

    val minimumRequired: Lens[Group, Option[NonNegShort]] =
      Focus[Group](_.minimumRequired)

    val ordered: Lens[Group, Boolean] = Focus[Group](_.ordered)

    val minimumInterval: Lens[Group, Option[TimeSpan]] =
      Focus[Group](_.minimumInterval)
    val maximumInterval: Lens[Group, Option[TimeSpan]] =
      Focus[Group](_.maximumInterval)

  case class Obs(id: Observation.Id) derives Eq

  object Obs:
    val id: Lens[Obs, Observation.Id] = Focus[Obs](_.id)
