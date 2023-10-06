// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.Eq
import cats.Order.*
import cats.derived.*
import cats.syntax.all.*
import eu.timepit.refined.cats.*
import explore.data.KeyedIndexedList
import explore.model.GroupList
import explore.model.GroupObs
import explore.model.Grouping
import explore.model.GroupingElement
import explore.model.ObsSummary
import lucuma.core.model.Observation
import lucuma.react.primereact.Tree
import lucuma.react.primereact.Tree.Node

enum ObsNode derives Eq:
  case Obs(value: ObsSummary)
  case And(group: Grouping)
  case Or(group: Grouping)

object ObsNode:

  def fromList(
    obsList: KeyedIndexedList[Observation.Id, ObsSummary],
    groups:  GroupList
  ): Seq[Node[ObsNode]] =

    val rootGroups =
      groups
        .mapFilter(g => if g.parentGroupId.isEmpty then g.value.some else none)
        .sortBy(_.fold(_.groupIndex, _.parentIndex))

    val groupings = KeyedIndexedList.fromList(groups.flatMap(_.value.toOption), _.id)

    def createObsNode(groupObs: GroupObs): Option[Node[ObsNode]] =
      obsList.getValue(groupObs.id).map(n => Tree.Node(Tree.Id(n.id.toString), Obs(n)))

    def createGroup(group: Grouping): Node[ObsNode] =
      val children = group.elements.sortBy(_.fold(_.groupIndex, _.parentIndex)).flatMap {
        case Left(groupObs)  => createObsNode(groupObs)
        case Right(grouping) => groupings.getValue(grouping.id).map(createGroup)
      }
      val isAnd    = group.minimumRequired.forall(_.value == group.elements.length)
      val data     = if isAnd then And(group) else Or(group)
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
    //     case id: GroupId        => pprint.Tree.Literal(pprint.Util.literalize(id.toString()))
    //     case id: Observation.Id => pprint.Tree.Literal(pprint.Util.literalize(id.toString()))
    //   })
    //   .pprintln(treeNodes)
    treeNodes
