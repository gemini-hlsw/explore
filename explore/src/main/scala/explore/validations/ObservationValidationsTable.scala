// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.validations

import cats.syntax.all.*
import explore.Icons
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.Focused
import explore.model.ObsSummary
import explore.model.ObservationList
import explore.model.enums.AppTab
import explore.model.reusability.given
import japgolly.scalajs.react.*
import japgolly.scalajs.react.ScalaFnComponent
import japgolly.scalajs.react.vdom.TagOf
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.ObservationValidationCode
import lucuma.core.model.Observation
import lucuma.core.model.ObservationValidation
import lucuma.core.model.Program
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.*
import lucuma.react.primereact.tooltip.*
import lucuma.react.resizeDetector.hooks.*
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.react.table.ColumnDef
import lucuma.react.table.ColumnId
import lucuma.ui.primereact.*
import lucuma.ui.table.*

import scala.scalajs.js

case class ObservationValidationsTable(
  programId:     Program.Id,
  observations:  ObservationList,
  renderInTitle: Tile.RenderInTitle
) extends ReactFnProps(ObservationValidationsTable.component)

object ObservationValidationsTable {
  import ValidationsTableRow.*

  private type Props = ObservationValidationsTable

  private val ColDef = ColumnDef[Expandable[ValidationsTableRow]]

  private val ObservationIdColumnId     = ColumnId("observation_id")
  private val ObservationTitleColumnId  = ColumnId("observation_title")
  private val ObservationStatusColumnId = ColumnId("observation_status")
  private val ValidationCodeColumnId    = ColumnId("validation_code")
  private val ValidationMessageColumnId = ColumnId("validation_message")

  private val columnNames: Map[ColumnId, String] = Map(
    ObservationIdColumnId     -> "Observation Id",
    ObservationTitleColumnId  -> "Title",
    ObservationStatusColumnId -> "Status",
    ValidationCodeColumnId    -> "Category",
    ValidationMessageColumnId -> "Validation"
  )

  private def messagesTailRows(
    obsId: Observation.Id,
    ov:    ObservationValidation
  ): List[Expandable[ValidationsTableRow]] =
    ov.messages.tail.map(m => Expandable(MessageRow(obsId, ov.code, m))).toList

  private def column[V](
    id:       ColumnId,
    accessor: ValidationsTableRow => V
  ): ColumnDef.Single.NoMeta[Expandable[ValidationsTableRow], V] =
    ColDef(id, r => accessor(r.value), columnNames(id))

  private val component = ScalaFnComponent
    .withHooks[Props]
    .useContext(AppContext.ctx)
    // columns
    .useMemoBy((_, _) => ()) { (props, ctx) => _ =>
      def obsUrl(obsId: Observation.Id): String    =
        ctx.pageUrl(AppTab.Observations, props.programId, Focused.singleObs(obsId))
      def goToObs(obsId: Observation.Id): Callback =
        ctx.pushPage(AppTab.Observations, props.programId, Focused.singleObs(obsId))

      def toggleAll(row: Row[Expandable[ValidationsTableRow], Nothing]): Callback =
        row.toggleExpanded() *> row.subRows.traverse(r => toggleAll(r)).void

      List(
        ColDef(
          ColumnId("expander"),
          cell = cell =>
            if (cell.row.original.value.isObsRow && cell.row.getCanExpand())
              <.span(
                ^.cursor.pointer,
                TableStyles.ExpanderChevron,
                TableStyles.ExpanderChevronOpen.when(cell.row.getIsExpanded()),
                ^.onClick ==> (_.stopPropagationCB *> toggleAll(cell.row))
              )(TableIcons.ChevronRight.withFixedWidth(true))
            else "",
          enableResizing = false
        ).setSize(30.toPx),
        column(ObservationIdColumnId, _.forObs(_.obs.id))
          .setCell(cell =>
            cell.value.map: oid =>
              <.a(^.href := obsUrl(oid),
                  ^.onClick ==> (_.preventDefaultCB *> goToObs(oid)),
                  oid.toString
              )
          )
          .setSize(50.toPx),
        column(ObservationStatusColumnId, _.forObs(_.obs.status)).setMaxSize(50.toPx),
        column(ObservationTitleColumnId, _.forObs(_.obs.title)).setCell(_.value),
        ColDef(
          ValidationCodeColumnId,
          cell = cell => cell.row.original.value.category(cell.row.getIsExpanded()),
          header = columnNames(ValidationCodeColumnId)
        ),
        ColDef(
          ValidationMessageColumnId,
          cell = cell => cell.row.original.value.message(cell.row.getIsExpanded()),
          header = columnNames(ValidationMessageColumnId)
        )
      )
    }
    // Rows
    .useMemoBy((props, _, _) => props.observations.toList)((_, _, _) =>
      _.filterNot(_.validations.isEmpty)
        .map(obs =>
          Expandable(
            ObsRow(obs),
            if (obs.validations.size > 1)
              // only include the tails for messages and validations. The head will be shown in the "parent" row.
              messagesTailRows(obs.id, obs.validations.head) ++
                obs.validations.tail
                  .map(v =>
                    Expandable(
                      ValidationRow(obs.id, v),
                      messagesTailRows(obs.id, v)
                    )
                  )
            else Nil
          )
        )
    )
    .useReactTableBy((_, _, cols, rows) =>
      TableOptions(
        cols,
        rows,
        enableExpanding = true,
        initialState = TableState(expanded = Expanded.AllRows),
        getSubRows = (row, _) => row.subRows,
        getRowId = (row, _, _) => RowId(row.value.rowId)
      )
    )
    .useResizeDetector()
    .render((props, _, rows, _, table, resizer) =>
      React.Fragment(
        props.renderInTitle(
          <.div(
            ExploreStyles.TableSelectionToolbar,
            Button(
              size = Button.Size.Small,
              icon = Icons.SquarePlus,
              tooltip = "Expand All",
              onClick = table.toggleAllRowsExpanded(true)
            ).compact,
            Button(
              size = Button.Size.Small,
              icon = Icons.SquareMinus,
              tooltip = "Collapse All",
              onClick = table.toggleAllRowsExpanded(false)
            ).compact
          )
        ),
        PrimeAutoHeightVirtualizedTable(
          table,
          _ => 32.toPx,
          striped = true,
          compact = Compact.Very,
          containerRef = resizer.ref,
          hoverableRows = rows.nonEmpty,
          emptyMessage = <.div("There are no Observation Errors.")
        )
      )
    )

  enum ValidationsTableRow {
    case ObsRow(
      obs: ObsSummary
    )

    case ValidationRow(
      obsId:      Observation.Id,
      validation: ObservationValidation
    )

    case MessageRow(
      obsId:   Observation.Id,
      code:    ObservationValidationCode,
      message: String
    )

    def isObsRow: Boolean =
      this match
        case r: ObsRow => true
        case _         => false

    def fold[A](f: ObsRow => A, g: ValidationRow => A, h: MessageRow => A): A =
      this match
        case r: ObsRow        => f(r)
        case r: ValidationRow => g(r)
        case r: MessageRow    => h(r)

    def forObs[A](f: ObsRow => A): js.UndefOr[A] =
      fold(r => f(r), _ => js.undefined, _ => js.undefined)

    def rowId: String =
      fold(_.obs.id.toString,
           r => s"${r.obsId}-${r.validation.code.tag}",
           r => s"${r.obsId}-${r.code.tag}-${r.message}"
      )

    private def categoryCell(validations: ObservationValidation*) =
      <.span(validations.map(_.code.name).mkString(", "))
        .withTooltip(content = <.div(validations.toList.toTagMod(ov => <.div(ov.code.description))))

    def category(isExpanded: Boolean): VdomElement =
      fold(
        r =>
          if (isExpanded) categoryCell(r.obs.validations.head) // head is safe here
          else categoryCell(r.obs.validations*),
        r => categoryCell(r.validation),
        _ => <.span()
      )

    def message(isExpanded: Boolean): String =
      fold(
        r =>
          if (isExpanded) r.obs.validations.headOption.map(_.messages.head).orEmpty
          else r.obs.validations.flatMap(_.messages.toList).mkString(", "),
        r =>
          if (isExpanded) r.validation.messages.head
          else r.validation.messages.mkString_(", "),
        _.message
      )
  }
}
