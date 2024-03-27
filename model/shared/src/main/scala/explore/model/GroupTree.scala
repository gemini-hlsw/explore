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
import lucuma.schemas.ObservationDB.Enums.Existence
import monocle.Focus
import monocle.Lens

type GroupTree = KeyedIndexedTree[GroupTree.Key, GroupTree.Value]

object GroupTree:

  type Key   = Either[Observation.Id, GroupId]
  type Index = KeyedIndexedTree.Index[Key]
  type Value = Either[Obs, Group]
  type Node  = TreeNode[Value]

  def fromList(groups: List[GroupElement]): GroupTree = {
    // For faster lookup when creating the tree
    val groupMap = groups
      .flatMap(_.value.toOption.map(group => (group.id, group)))
      .toMap

    def createObsNode(obs: GroupObs): Option[Node] =
      obs.existence.filter(_ === Existence.Present).as(TreeNode(Obs(obs.id).asLeft[Group], Nil))

    def createGroupNode(group: Grouping): Node =
      val children = group.elements
        .sortBy(_.groupIndex)
        .flatMap(
          _.fold(
            createObsNode(_),
            group => groupMap.get(group.id).map(createGroupNode)
          )
        )
      TreeNode(group.toGroupTreeGroup.asRight, children)

    val rootGroups =
      groups
        .mapFilter(g => if g.parentGroupId.isEmpty then g.value.some else none)
        .sortBy(_.groupIndex)

    val nodes = rootGroups.flatMap(_.fold(createObsNode, g => createGroupNode(g).some))

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
    val name: Lens[Group, Option[NonEmptyString]] = Focus[Group](_.name)

    val minimumRequired: Lens[Group, Option[NonNegShort]] =
      Focus[Group](_.minimumRequired)

    val ordered: Lens[Group, Boolean] = Focus[Group](_.ordered)

    val minimumInterval: Lens[Group, Option[TimeSpan]] =
      Focus[Group](_.minimumInterval)
    val maximumInterval: Lens[Group, Option[TimeSpan]] =
      Focus[Group](_.maximumInterval)

  case class Obs(id: Observation.Id) derives Eq
