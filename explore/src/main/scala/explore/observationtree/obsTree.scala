// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.Eq
import cats.Order.*
import cats.derived.*
import cats.syntax.all.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.numeric.NonNegShort
import explore.data.KeyedIndexedList
import explore.data.tree.KeyedIndexedTree
import explore.data.tree.Node as IndexNode
import explore.data.tree.Tree as IndexTree
import explore.model.*
import explore.model.syntax.all.*
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.react.primereact.Tree
import lucuma.react.primereact.Tree.Node
import monocle.Iso

enum ObsNode derives Eq:
  case Obs(obs: ObsSummary)
  case And(group: Grouping)
  case Or(group: Grouping)

  def id: Either[Observation.Id, Group.Id] = this match
    case Obs(obs)   => obs.id.asLeft
    case And(group) => group.id.asRight
    case Or(group)  => group.id.asRight

  def value: Either[ObsSummary, Grouping] = this match
    case Obs(value) => value.asLeft
    case And(group) => group.asRight
    case Or(group)  => group.asRight

  def isObs: Boolean = this match
    case Obs(_) => true
    case _      => false

  def isGroup: Boolean = !isObs

object ObsNode:

  def toNodes(
    obsList: ObservationList,
    groups:  GroupList
  ): Seq[Node[ObsNode]] =

    val rootGroups =
      groups
        .mapFilter(g => if g.parentGroupId.isEmpty then g.value.some else none)
        .sortBy(_.groupIndex)

    val groupings = KeyedIndexedList.fromList(groups.flatMap(_.value.toOption), _.id)

    def createObsNode(groupObs: GroupObs): Option[Node[ObsNode]] =
      obsList.getValue(groupObs.id).map(n => Tree.Node(Tree.Id(n.id.toString), Obs(n)))

    def createGroup(group: Grouping): Node[ObsNode] =
      val children = group.elements.sortBy(_.groupIndex).flatMap {
        case Left(groupObs)  => createObsNode(groupObs)
        case Right(grouping) => groupings.getValue(grouping.id).map(createGroup)
      }
      val data     = if group.isAnd then And(group) else Or(group)
      Tree.Node(Tree.Id(group.id.toString), data, children = children)

    val treeNodes = rootGroups.flatMap {

      case Left(groupObs)  => createObsNode(groupObs)
      case Right(grouping) => groupings.getValue(grouping.id).map(createGroup)

    }

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

  def fromNodes(nodes: Seq[Node[ObsNode]]): (ObservationList, GroupList) = {

    def nodesToTree(nodes: Seq[Node[ObsNode]]): List[IndexNode[ObsNode]] =
      nodes.map(node => IndexNode(node.data, nodesToTree(node.children))).toList

    val keyedNodeTree =
      KeyedIndexedTree.fromTree(IndexTree(nodesToTree(nodes)), _.id)

    val newObsList = KeyedIndexedList.fromList(
      keyedNodeTree.collect { case (_, IndexNode(Obs(obs), _), index) =>
        ObsSummary.groupId
          .replace(index.parentKey.flatMap(_.toOption))
          .andThen(ObsSummary.groupIndex.replace(NonNegShort.unsafeFrom(index.childPos.toShort)))
          .apply(obs)
      },
      _.id
    )

    val newGroups = keyedNodeTree.collect { case (_, IndexNode(group, children), index) =>
      val newGroupId = index.parentKey.flatMap(_.toOption)
      val newIndex   = NonNegShort.unsafeFrom(index.childPos.toShort)
      GroupElement.parentGroupId
        .replace(newGroupId)
        .andThen(
          GroupElement.value.modify(
            _.bimap(
              GroupObs.groupIndex.replace(newIndex),
              Grouping.parentIndex
                .replace(newIndex)
                .andThen(
                  Grouping.elements.replace(
                    children.zipWithIndex
                      .map((el, i) =>
                        val newIndex = NonNegShort.unsafeFrom(i.toShort)
                        el.value.value.bimap(
                          obs =>
                            GroupObs(
                              obs.id,
                              newIndex
                            ),
                          group => GroupingElement(group.id, newIndex)
                        )
                      )
                      .toList
                  )
                )
            )
          )
        )(toGroupEl(group.value))
    }

    newObsList -> newGroups
  }

  val obsGroupNodeIso: Iso[(ObservationList, GroupList), Seq[Node[ObsNode]]] =
    Iso[(ObservationList, GroupList), Seq[Node[ObsNode]]](toNodes)(fromNodes)

  private def toGroupEl(group: Either[ObsSummary, Grouping]): GroupElement = GroupElement(
    group.leftMap(o => GroupObs(o.id, o.groupIndex)),
    group.fold(_.groupId, _.parentId)
  )
