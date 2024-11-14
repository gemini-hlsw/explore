// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.syntax.all.*
import crystal.Pot
import explore.components.Tile
import explore.components.TileController
import explore.components.ui.ExploreStyles
import explore.model.Group
import explore.model.GroupEditTileIds
import explore.model.ProgramTimeRange
import explore.model.enums.GridLayoutSection
import explore.model.layout.LayoutsMap
import explore.undo.UndoSetter
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.*
import lucuma.react.common.*
import lucuma.react.common.ReactFnProps
import lucuma.react.resizeDetector.UseResizeDetectorReturn

case class ObsGroupTiles(
  userId:            Option[User.Id],
  group:             UndoSetter[Group],
  childCount:        Int,
  timeEstimateRange: Pot[Option[ProgramTimeRange]],
  resize:            UseResizeDetectorReturn,
  defaultLayouts:    LayoutsMap,
  layouts:           LayoutsMap,
  backButton:        VdomNode
) extends ReactFnProps(ObsGroupTiles.component)

object ObsGroupTiles:
  private type Props = ObsGroupTiles

  val component = ScalaFnComponent
    .withHooks[Props]
    .render: props =>
      val editTile: Tile[?] =
        Tile(
          GroupEditTileIds.GroupEditId.id,
          s"${
              if props.group.get.system then props.group.get.name.foldMap(_.value)
              else if props.group.get.isAnd then "AND"
              else "OR"
            } Group",
          props.backButton.some,
          tileTitleClass = ExploreStyles.GroupEditTitle
        )(
          _ =>
            GroupEditBody(
              props.group,
              props.childCount,
              props.timeEstimateRange,
              props.group.get.system
            )
              .withKey(props.group.get.id.toString)
              .toUnmounted,
          (_, _) => GroupEditTitle(props.group, props.childCount, props.timeEstimateRange)
        )

      val notesTile = Tile(
        GroupEditTileIds.GroupNotesId.id,
        s"Note for Observer"
      )(_ =>
        <.div(
          ExploreStyles.NotesTile,
          <.div(
            ExploreStyles.ObserverNotes,
            "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Phasellus maximus hendrerit lacinia. Etiam dapibus blandit ipsum sed rhoncus."
          )
        )
      )

      TileController(
        props.userId,
        props.resize.width.orEmpty,
        props.defaultLayouts,
        props.layouts,
        List(editTile, notesTile),
        GridLayoutSection.GroupEditLayout,
        props.backButton.some
      )
