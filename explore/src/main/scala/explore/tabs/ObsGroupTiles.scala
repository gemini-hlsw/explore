// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.syntax.all.*
import crystal.Pot
import explore.components.Tile
import explore.components.TileController
import explore.components.ui.ExploreStyles
import explore.model.GroupEditIds
import explore.model.GroupTree
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
  groupId:           Group.Id,
  groups:            UndoSetter[GroupTree],
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

      val tiles =
        if !props.groups.get.contains(props.groupId.asRight) then List.empty
        else
          val groupTreeKey = props.groupId.asRight
          // First zoom to the node, to get the number of child elements
          // We can safely .get here because we know the key is in the tree and that the value is a Grouping
          val node         = props.groups
            .zoom(
              _.getNodeAndIndexByKey(groupTreeKey).get,
              modF =>
                groups =>
                  groups
                    .getNodeAndIndexByKey(groupTreeKey)
                    .map(modF)
                    .map((newValue, newIndex) =>
                      groups.updated(groupTreeKey, newValue.value, newIndex)
                    )
                    .getOrElse(groups)
            )

          // Then zoom to the Grouping itself
          val group =
            node.zoom(_._1.value.toOption.get,
                      modF => _.leftMap(node => node.copy(value = node.value.map(modF)))
            )

          val editTile = Tile(
            GroupEditIds.GroupEditId.id,
            s"${if group.get.isAnd then "AND" else "OR"} Group",
            props.backButton.some,
            tileTitleClass = ExploreStyles.GroupEditTitle
          )(
            _ =>
              GroupEditBody(
                group,
                node.get._1.children.length,
                props.timeEstimateRange,
                group.get.system
              )
                .withKey(props.groupId.toString)
                .toUnmounted,
            (_, _) => GroupEditTitle(group, node.get._1.children.length, props.timeEstimateRange)
          )

          val notesTile = Tile(
            GroupEditIds.GroupNotesId.id,
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

          List(editTile, notesTile)

      TileController(
        props.userId,
        props.resize.width.orEmpty,
        props.defaultLayouts,
        props.layouts,
        tiles,
        GridLayoutSection.GroupEditLayout,
        props.backButton.some
      )
