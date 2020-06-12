// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.implicits._
import cats.effect.IO
import explore.implicits._
import java.util.UUID
import japgolly.scalajs.react._
import explore.data.tree._
import react.atlasKit.tree.{ Tree => AtlasTree }
import japgolly.scalajs.react.vdom.html_<^._
import mouse.boolean._
import explore.undo.TreeMod
import explore.model.ExploreObservation
import explore.model.SiderealTarget
import explore.model.enum.ObsStatus
import scala.util.Random
import java.time.Duration
import java.time.temporal.ChronoUnit._
import explore.components.ObsBadge
import react.semanticui.elements.icon.Icon

object AndOrTest {
  private def randomElement[A](list: List[A]): A =
    list(
      Random.between(0, list.length)
    )

  def obs(targetName: String): ObsNode =
    ObsNode.Obs(
      UUID.randomUUID,
      ExploreObservation(
        UUID.randomUUID,
        SiderealTarget(targetName, null),
        randomElement(ObsStatus.ObsStatusEnumerated.all),
        "GMOS-N R831 1x 300",
        "<0.8\" <0.3 mag Gray",
        Duration.of(Random.between(30L, 200L), MINUTES)
      )
    )

  def or(orParams: String): ObsNode =
    ObsNode.Or(UUID.randomUUID, orParams)

  def and(andParams: String): ObsNode =
    ObsNode.And(UUID.randomUUID, andParams)

  val initialTree: Tree[ObsNode] =
    Tree(
      Node(obs("NGC 1055")),
      Node(obs("NGC 7752")),
      Node(obs("NGC 1068")),
      Node(or(""),
           Node(obs("NGC 1087")),
           Node(and(""), Node(obs("NGC 1087")), Node(obs("NGC 1087")))
      )
    )

  val obsTreeMod = new TreeMod[IO, ObsNode, UUID](ObsNode.id)

  def wrap(divAttrs: TagMod*)(element: VdomNode, overlay: VdomNode): VdomNode =
    <.div(
      <.div(^.position.absolute, ^.zIndex := "2", ^.pointerEvents.none)(divAttrs: _*)(overlay),
      <.div(divAttrs: _*)(element)
    )

  def renderObs(obs: ExploreObservation, dragIcon: VdomNode): VdomNode =
    wrap(^.width := "200px", ^.margin := "5px")(
      ObsBadge(obs, ObsBadge.Layout.NameAndConf),
      <.div(^.textAlign.right, dragIcon)
    )

  def renderOp(opIcon: VdomNode, text: String, dragIcon: VdomNode): VdomNode =
    wrap(^.width := "235px", ^.marginLeft := "5px")(
      <.span(opIcon, text),
      <.div(^.textAlign.right, dragIcon)
    )

  def renderItem(params: AtlasTree.RenderItemParams[ObsNode]): VdomNode = {
    def handleClick: Callback =
      params.item.isExpanded
        .forall(identity)
        .fold(params.onCollapse(params.item.id), params.onExpand(params.item.id))

    val opIcon   = Icon(
      "chevron " + params.item.isExpanded.forall(identity).fold("down", "right")
    )(^.cursor.pointer, ^.onClick --> handleClick)

    val dragIcon = <.span(params.provided.dragHandleProps, ^.pointerEvents.all, Icon("sort"))

    <.div(params.provided.innerRef, params.provided.draggableProps)(
      params.item.data
        .map[VdomNode](_ match {
          case ObsNode.Obs(_, obs) => renderObs(obs, dragIcon)
          case ObsNode.And(_, _)   =>
            renderOp(opIcon, "AND", dragIcon)
          case ObsNode.Or(_, _)    =>
            renderOp(opIcon, "OR", dragIcon)
        })
        .getOrElse("<ROOT>")
    )
  }

  def render: VdomElement =
    TreeComp[ObsNode, UUID](initialTree, ObsNode.id.get, UUID.fromString, obsTreeMod, renderItem)
}
