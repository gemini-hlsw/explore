// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.syntax.all.*
import explore.components.Tile
import explore.components.TileController
import explore.model.GroupElement
import explore.model.GroupList
import explore.model.enums.GridLayoutSection
import explore.model.layout.LayoutsMap
import explore.undo.UndoSetter
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.*
import lucuma.react.common.ReactFnProps
import lucuma.react.common.*
import lucuma.react.resizeDetector.UseResizeDetectorReturn
import lucuma.refined.*
import monocle.Traversal

case class ObsGroupTiles(
  userId:         Option[User.Id],
  groupId:        Group.Id,
  groups:         UndoSetter[GroupList],
  resize:         UseResizeDetectorReturn,
  defaultLayouts: LayoutsMap,
  layouts:        LayoutsMap,
  backButton:     VdomNode
) extends ReactFnProps(ObsGroupTiles.component)

object ObsGroupTiles:
  private given Reusability[GroupElement] = Reusability.byEq
  private given Reusability[Group.Id]     = Reusability.byEq

  private type Props = ObsGroupTiles
  def component = ScalaFnComponent
    .withHooks[Props]
    .useMemoBy(props => (props.groupId, props.groups.get))(_ =>
      (groupId, groups) =>
        val group = Traversal
          .fromTraverse[List, GroupElement]
          .andThen(GroupElement.grouping)
          .find(_.id === groupId)(groups)
        group.get
    )
    .render { (props, group) =>

      val editTile = Tile(
        "group-edit".refined,
        s"${if group.value.isAnd then "AND" else "OR"} Group",
        props.backButton.some
      )(renderInTitle =>
        React.Fragment(
          renderInTitle("hello"),
          <.div("GroupEditTiles"),
          props.groupId.show
        )
      )

      val notesTile =
        Tile(
          ObsTabTilesIds.NotesId.id,
          s"Note for Observer",
          canMinimize = true
        )(_ =>
          <.div(
            ExploreStyles.NotesWrapper,
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

    }
