// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Order.*
import cats.syntax.all.*
import eu.timepit.refined.cats.*
import explore.data.tree.KeyedIndexedTree
import explore.data.tree.Node
import explore.data.tree.Tree
import explore.model.syntax.all.*
import lucuma.core.model.Group
import lucuma.core.model.Observation

type GroupTree = KeyedIndexedTree[Either[Observation.Id, Group.Id], Either[GroupObs, Grouping]]

object GroupTree:
  def fromList(groups: GroupList): GroupTree = {
    // For faster lookup when creating the tree
    val groupMap = groups
      .flatMap(_.value.toOption.map(group => (group.id, group)))
      .toMap

    def createObsNode(obs: GroupObs): Node[Either[GroupObs, Grouping]] =
      Node(obs.asLeft[Grouping], Nil)

    def createGroupNode(group: Grouping): Node[Either[GroupObs, Grouping]] =
      val children = group.elements
        .sortBy(_.groupIndex)
        .flatMap(
          _.fold(
            createObsNode(_).some,
            group => groupMap.get(group.id).map(createGroupNode)
          )
        )
      Node(group.asRight, children)

    val rootGroups =
      groups
        .mapFilter(g => if g.parentGroupId.isEmpty then g.value.some else none)
        .sortBy(_.groupIndex)

    val nodes = rootGroups.map(_.fold(createObsNode, createGroupNode))

    KeyedIndexedTree.fromTree(Tree(nodes), _.bimap(_.id, _.id))
  }
