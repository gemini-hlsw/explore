// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import explore.data.KeyedIndexedList
import explore.model.ObsSummary
import japgolly.scalajs.react.Reusability
import lucuma.core.model.Group.{Id => GroupId}
import lucuma.core.model.Observation
import lucuma.refined.*
import lucuma.ui.reusability.given
import monocle.Lens
import monocle.macros.GenLens
import queries.common.ProgramQueriesGQL.ProgramGroupsQuery.Data.Program.AllGroupElements
import queries.common.ProgramQueriesGQL.ProgramGroupsQuery.Data.Program.AllGroupElements.Group
import queries.common.ProgramQueriesGQL.ProgramGroupsQuery.Data.Program.AllGroupElements.Group.Elements
import react.primereact.Tree
import react.primereact.Tree.Node

import java.util.UUID
import scala.scalajs.js.JSConverters._

enum ObsNode derives Eq:
  case Obs(value: ObsSummary)
  case And(group: Group)
  case Or(group: Group)

object ObsNode {

  def fromList(
    obsList: KeyedIndexedList[Observation.Id, ObsSummary],
    groups:  List[AllGroupElements]
  ): Seq[Node[ObsNode]] =

    val rootGroups = groups.filter(_.parentGroupId.isEmpty)
    val groupings  = groups.flatMap(_.group)

    def createGroupFromIds(
      maybeGroup: Option[GroupId],
      maybeObs:   Option[Observation.Id]
    ): Seq[Node[ObsNode]] =
      val groupNodes =
        maybeGroup.flatMap(group => groupings.find(_.id === group).map(createGroup))
      val obsNodes   = maybeObs.flatMap(groupObs =>
        obsList.getValue(groupObs).map(n => Tree.Node(Tree.Id(n.id.toString), Obs(n)))
      )
      (groupNodes ++ obsNodes).toSeq

    def createGroup(group: Group): Node[ObsNode] =
      val children = group.elements.flatMap { case Elements(maybeGroup, maybeObs) =>
        createGroupFromIds(maybeGroup.map(_.id), maybeObs.map(_.id))
      }
      // is and if minimumRequired is not defined, or same length as group.elements
      val isAnd    = group.minimumRequired.forall(_.value == group.elements.length)
      val data     = if isAnd then And(group) else Or(group)
      Tree.Node(Tree.Id(group.id.toString), data, children = children)

    val treeNodes = rootGroups.flatMap { case AllGroupElements(maybeObs, maybeGroup, _) =>
      createGroupFromIds(maybeGroup.map(_.id), maybeObs.map(_.id))
      val groupNode = maybeGroup
        .flatMap(group => groupings.find(g => g.id === group.id).map(createGroup))
      val obsNode   = maybeObs
        .flatMap(groupObs =>
          obsList.getValue(groupObs.id).map(n => Tree.Node(Tree.Id(n.id.toString), Obs(n)))
        )
      groupNode ++ obsNode
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

}
