// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.effect.IO
import cats.syntax.all._
import eu.timepit.refined.auto._
import eu.timepit.refined.types.string.NonEmptyString
import explore.components.ObsBadge
import explore.data.tree._
import explore.model.enum.{ ObsStatus, _ }
import explore.model.{ Constraints, ExploreObservation, ObsSummary, SiderealTarget }
import explore.undo.KITreeMod
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.math.{ Coordinates, Epoch }
import lucuma.core.model.{ Observation, SiderealTracking }
import mouse.boolean._
import react.atlasKit.tree.{ Tree => AtlasTree }
import react.semanticui.elements.icon.Icon

import java.time.Duration
import java.time.temporal.ChronoUnit._
import java.util.UUID
import scala.util.Random

object AndOrTest {
  private def randomElement[A](list: List[A]): A =
    list(
      Random.between(0, list.length)
    )

  val constraints =
    Constraints(UUID.randomUUID,
                "<0.8\" <0.3 mag Gray",
                CloudCover.Any,
                ImageQuality.Any,
                SkyBackground.Any,
                WaterVapor.Any
    )

  def obs(targetName: NonEmptyString): ObsNode =
    ObsNode.Obs(
      UUID.randomUUID,
      ExploreObservation(
        UUID.randomUUID,
        SiderealTarget(UUID.randomUUID,
                       targetName,
                       SiderealTracking(none, Coordinates.Zero, Epoch.J2000, none, none, none)
        ),
        randomElement(ObsStatus.ObsStatusEnumerated.all),
        "GMOS-N R831 1x 300",
        constraints,
        Duration.of(Random.between(30L, 200L), MINUTES)
      )
    )

  def or(orParams: String): ObsNode =
    ObsNode.Or(UUID.randomUUID, orParams)

  def and(andParams: String): ObsNode =
    ObsNode.And(UUID.randomUUID, andParams)

  val initialTree: KeyedIndexedTree[UUID, ObsNode] =
    KeyedIndexedTree.fromTree(
      Tree(
        Node(obs("NGC 1055")),
        Node(obs("NGC 7752")),
        Node(obs("NGC 1068")),
        Node(or(""),
             Node(obs("NGC 1087")),
             Node(and(""), Node(obs("NGC 1087")), Node(obs("NGC 1087")))
        )
      ),
      ObsNode.id.get
    )

  val obsTreeMod = new KITreeMod[IO, ObsNode, UUID](ObsNode.id)

  def wrap(divAttrs: TagMod*)(element: VdomNode, overlay: VdomNode): VdomNode =
    <.div(
      <.div(^.position.absolute, ^.zIndex := "2", ^.pointerEvents.none)(divAttrs: _*)(overlay),
      <.div(divAttrs: _*)(element)
    )

  def renderObs(obs: ExploreObservation, dragIcon: VdomNode): VdomNode =
    wrap(^.width := "200px", ^.margin := "5px")(
      ObsBadge(
        ObsSummary(id = Observation.Id.parse(s"o-${obs.id.toString.filter(_.isDigit).take(4)}").get,
                   name = obs.target.name.value.some,
                   observationTarget = None
        ),
        ObsBadge.Layout.NameAndConf
      ),
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

    <.div(params.provided.innerRef, params.provided.draggableProps, params.provided.draggableStyle)(
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
    TreeComp[UUID, ObsNode](initialTree, _.toString, UUID.fromString, obsTreeMod, renderItem)
}
