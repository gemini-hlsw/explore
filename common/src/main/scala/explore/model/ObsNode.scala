// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.Order.*
import cats.derived.*
import cats.syntax.all.*
import explore.data.KeyedIndexedList
import explore.data.tree.KeyedIndexedTree
import explore.data.tree.Node as IndexNode
import explore.data.tree.Tree as IndexTree
import explore.model.*
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.react.primereact.Tree
import lucuma.react.primereact.Tree.Node
import monocle.Iso

enum ObsNode derives Eq:
  case Obs(obs: ObsSummary)
  case Grp(group: Grouping)

  def id: Either[Observation.Id, Group.Id] = this match
    case Obs(obs)   => obs.id.asLeft
    case Grp(group) => group.id.asRight

  def value: Either[ObsSummary, Grouping] = this match
    case Obs(value) => value.asLeft
    case Grp(group) => group.asRight

  def isObs: Boolean   = this match
    case Obs(_) => true
    case _      => false
  def isGroup: Boolean = !isObs

object ObsNode:

  def toNodes(
    obsList: ObservationList,
    groups:  GroupTree
  ): List[Node[ObsNode]] =

    /**
     * Recursively create a tree of PrimeReact Tree Nodes from a GroupTree Node
     *
     * @return
     *   Option[Node[ObsNode]] Option because deleted observations are in the GroupTree, but not in
     *   the ObservationList, they are filtered out here
     */
    def createNode(node: IndexNode[Either[GroupObs, Grouping]]): Option[Node[ObsNode]] =
      node.value match
        case Left(obs)    =>
          obsList.getValue(obs.id).map(obs => Tree.Node(Tree.Id(obs.id.toString), Obs(obs)))
        case Right(group) =>
          val children = node.children.flatMap(createNode)
          val data     = Grp(group)
          Tree.Node(Tree.Id(group.id.toString), data, children = children).some

    val treeNodes = groups.toTree.children.flatMap(createNode)

    // Uncomment to debug trees
    // pprint
    //   .copy(additionalHandlers = {
    //     case _: ObsSummary      =>
    //       pprint.Tree.Apply("ObsSummary", List(pprint.Tree.Literal("...")).iterator)
    //     case id: Group.Id        => pprint.Tree.Literal(pprint.Util.literalize(id.toString()))
    //     case id: Observation.Id => pprint.Tree.Literal(pprint.Util.literalize(id.toString()))
    //   })
    //   .pprintln(treeNodes)
    treeNodes

  def fromNodes(nodes: List[Node[ObsNode]]): (ObservationList, GroupTree) = {

    def nodesToTree(nodes: List[Node[ObsNode]]): List[IndexNode[Either[GroupObs, Grouping]]] =
      nodes
        .map(node =>
          IndexNode(
            node.data.value.leftMap(obs => GroupObs(obs.id, obs.groupIndex)),
            nodesToTree(node.children.toList)
          )
        )

    def collectObsNodes(acc: List[ObsSummary], node: Node[ObsNode]): List[ObsSummary] =
      node.data match
        case Obs(obs)   => obs +: acc
        case Grp(group) => node.children.foldLeft(acc)(collectObsNodes)

    val groupTree: GroupTree =
      KeyedIndexedTree.fromTree(IndexTree(nodesToTree(nodes)), _.bimap(_.id, _.id))

    val obsList: ObservationList = KeyedIndexedList.fromList(
      nodes.flatMap(collectObsNodes(Nil, _)),
      _.id
    )

    obsList -> groupTree
  }

  val obsGroupNodeIso: Iso[(ObservationList, GroupTree), List[Node[ObsNode]]] =
    Iso[(ObservationList, GroupTree), List[Node[ObsNode]]](toNodes)(fromNodes)
