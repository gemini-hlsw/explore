// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.data.NonEmptySet
import cats.syntax.all.*
import explore.components.Tile
import explore.components.TileController
import explore.components.ui.ExploreStyles
import explore.model.Group
import explore.model.GroupEditTileIds
import explore.model.enums.GridLayoutSection
import explore.model.enums.GroupWarning
import explore.model.layout.LayoutsMap
import explore.undo.UndoSetter
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.*
import lucuma.react.common.*
import lucuma.react.resizeDetector.UseResizeDetectorReturn

case class ObsGroupTiles(
  userId:         Option[User.Id],
  group:          UndoSetter[Group],
  groupWarnings:  Option[NonEmptySet[GroupWarning]],
  childCount:     Int,
  resize:         UseResizeDetectorReturn,
  defaultLayouts: LayoutsMap,
  layouts:        LayoutsMap,
  readonly:       Boolean,
  backButton:     VdomNode
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
              props.groupWarnings,
              props.childCount,
              props.group.get.system || props.readonly
            )
              .withKey(props.group.get.id.toString)
              .toUnmounted,
          (_, _) => GroupEditTitle(props.group, props.childCount)
        )

      TileController(
        props.userId,
        props.resize.width.orEmpty,
        props.defaultLayouts,
        props.layouts,
        List(editTile),
        GridLayoutSection.GroupEditLayout,
        props.backButton.some
      )
