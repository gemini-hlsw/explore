// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.validations

import cats.data.NonEmptySet
import cats.effect.IO
import cats.syntax.all.*
import crystal.react.View
import explore.Icons
import explore.common.UserPreferencesQueries.TableStore
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.Focused
import explore.model.Group
import explore.model.GroupList
import explore.model.OverviewTabTileIds
import explore.model.enums.AppTab
import explore.model.enums.GroupWarning
import explore.model.enums.TableId
import explore.model.enums.TileSizeState
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.core.util.NewType
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.*
import lucuma.react.resizeDetector.hooks.*
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.ui.primereact.*
import lucuma.ui.reusability.given
import lucuma.ui.table.*
import lucuma.ui.table.hooks.*

import scala.scalajs.js

object GroupWarningsTile {
  object TileState extends NewType[Boolean => Callback]
  type TileState = TileState.Type

  def apply(
    userId:        Option[User.Id],
    programId:     Program.Id,
    groups:        GroupList,
    groupWarnings: Map[Group.Id, NonEmptySet[GroupWarning]]
  ) =
    Tile(
      id = OverviewTabTileIds.GroupWarningsId.id,
      title = "Group Warnings",
      TileState(_ => Callback.empty)
    )(
      Body(userId, programId, groups, groupWarnings, _),
      Title.apply
    )

  private val ColDef = ColumnDef[Expandable[GroupWarningRow]]

  private val expanderColumnId  = ColumnId("expander")
  private val GroupIdColumnId   = ColumnId("group_id")
  private val GroupNameColumnId = ColumnId("group_name")
  private val WarningColumnId   = ColumnId("warning")

  private val columnNames: Map[ColumnId, String] = Map(
    GroupIdColumnId   -> "Group Id",
    GroupNameColumnId -> "Group Name",
    WarningColumnId   -> "Warning"
  )

  private def column[V](id: ColumnId, accessor: GroupWarningRow => V): ColDef.TypeFor[V] =
    ColDef(id, r => accessor(r.value), columnNames(id))

  private def columns(programId: Program.Id, ctx: AppContext[IO]): List[ColDef.Type] =
    def groupUrl(groupId: Group.Id): String =
      ctx.pageUrl((AppTab.Observations, programId, Focused.group(groupId)).some)

    def goToGroup(groupId: Group.Id): Callback =
      ctx.pushPage((AppTab.Observations, programId, Focused.group(groupId)).some)

    def toggleAll(row: Row[Expandable[GroupWarningRow], Nothing, Nothing, Nothing]): Callback =
      row.toggleExpanded() *> row.subRows.traverse(r => toggleAll(r)).void

    List(
      ColDef(
        expanderColumnId,
        cell = cell =>
          if (cell.row.original.value.isGroupRow && cell.row.getCanExpand())
            <.span(
              ^.cursor.pointer,
              TableStyles.ExpanderChevron,
              TableStyles.ExpanderChevronOpen.when(cell.row.getIsExpanded()),
              ^.onClick ==> (_.stopPropagationCB *> toggleAll(cell.row))
            )(TableIcons.ChevronRight.withFixedWidth(true))
          else "",
        enableResizing = false
      ).withSize(30.toPx),
      column(GroupIdColumnId, _.forGroup(_.group.id))
        .withCell(cell =>
          cell.value.map: gid =>
            <.a(^.href := groupUrl(gid),
                ^.onClick ==> (_.preventDefaultCB *> goToGroup(gid)),
                gid.toString
            )
        )
        .withSize(50.toPx),
      column(GroupNameColumnId, _.forGroup(_.group.name.map(_.value).orEmpty))
        .withCell(_.value)
        .withSize(50.toPx),
      ColDef(
        WarningColumnId,
        cell = cell => cell.row.original.value.message(cell.row.getIsExpanded()),
        header = columnNames(WarningColumnId)
      )
    )

  private case class Body(
    userId:        Option[User.Id],
    programId:     Program.Id,
    groups:        GroupList,
    groupWarnings: Map[Group.Id, NonEmptySet[GroupWarning]],
    tileState:     View[TileState]
  ) extends ReactFnProps(Body)

  private object Body
      extends ReactFnComponent[Body](props =>
        import GroupWarningRow.*

        given Reusability[GroupList] = Reusability.byEq

        for {
          ctx    <- useContext(AppContext.ctx)
          cols   <- useMemo(()): _ =>
                      columns(props.programId, ctx)
          rows   <- useMemo((props.groups, props.groupWarnings.toList)): (groups, warnings) =>
                      warnings
                        .map: gw =>
                          groups
                            .get(gw._1)
                            .map: group =>
                              Expandable(
                                GroupRow(group, gw._2),
                                // only include the tails of the warnings, since we'll
                                // show the head in the group row
                                gw._2.tail.toList.map: ws =>
                                  Expandable(WarningRow(gw._1, ws))
                              )
                        .flattenOption
          table  <- useReactTableWithStateStore:
                      import ctx.given

                      TableOptionsWithStateStore(
                        TableOptions(
                          cols,
                          rows,
                          enableExpanding = true,
                          initialState = TableState(expanded = Expanded.AllRows),
                          getSubRows = (row, _) => row.subRows,
                          getRowId = (row, _, _) => RowId(row.value.rowId)
                        ),
                        TableStore(
                          props.userId,
                          TableId.GroupWarnings,
                          cols
                        )
                      )
          _      <- useEffectOnMount:
                      val cb = (a: Boolean) => table.toggleAllRowsExpanded(a)
                      props.tileState.set(TileState(cb))
          resize <- useResizeDetector
        } yield PrimeAutoHeightVirtualizedTable(
          table,
          _ => 32.toPx,
          striped = true,
          compact = Compact.Very,
          containerRef = resize.ref,
          hoverableRows = rows.nonEmpty,
          emptyMessage = <.div("There are no Group Warnings.")
        )
      )

  private case class Title(tileState: View[TileState], tileSize: TileSizeState)
      extends ReactFnProps(Title)

  private object Title
      extends ReactFnComponent[Title](props =>
        if (props.tileSize === TileSizeState.Minimized)
          EmptyVdom
        else
          <.div(
            ExploreStyles.TableSelectionToolbar,
            Button(
              size = Button.Size.Small,
              icon = Icons.SquarePlus,
              tooltip = "Expand All",
              onClick = props.tileState.get.value(true)
            ).compact,
            Button(
              size = Button.Size.Small,
              icon = Icons.SquareMinus,
              tooltip = "Collapse All",
              onClick = props.tileState.get.value(false)
            ).compact
          )
      )

  private enum GroupWarningRow {
    case GroupRow(group: Group, warnings: NonEmptySet[GroupWarning])
    case WarningRow(groupId: Group.Id, warning: GroupWarning)

    def isGroupRow: Boolean = this match {
      case _: GroupRow   => true
      case _: WarningRow => false
    }

    def fold[A](f: GroupRow => A, g: WarningRow => A): A = this match {
      case r: GroupRow   => f(r)
      case r: WarningRow => g(r)
    }

    def rowId: String =
      fold(
        r => r.group.id.toString,
        r => s"${r.groupId}-${r.warning.shortMsg}"
      )

    def forGroup[A](f: GroupRow => A): js.UndefOr[A] = fold(f, _ => js.undefined)

    def message(isExpanded: Boolean): String =
      fold(
        r =>
          if (isExpanded || r.warnings.size == 1) r.warnings.head.longMsg
          else r.warnings.toList.map(_.shortMsg).mkString(", "),
        r => r.warning.longMsg
      )
  }
}
