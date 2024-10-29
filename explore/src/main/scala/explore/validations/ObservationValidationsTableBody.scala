// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.validations

import cats.effect.IO
import cats.syntax.all.*
import crystal.react.View
import crystal.react.syntax.effect.*
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.Focused
import explore.model.Observation
import explore.model.ObservationList
import explore.model.enums.AppTab
import explore.model.enums.TileSizeState
import explore.model.reusability.given
import japgolly.scalajs.react.*
import japgolly.scalajs.react.ScalaFnComponent
import japgolly.scalajs.react.vdom.TagOf
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.ObservationValidationCode
import lucuma.core.model.ObservationValidation
import lucuma.core.model.Program
import lucuma.core.util.NewType
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
import queries.schemas.odb.ObsQueries

import scala.scalajs.js

object ObservationValidationsTableTileState extends NewType[Boolean => Callback]
type ObservationValidationsTableTileState = ObservationValidationsTableTileState.Type

case class ObservationValidationsTableBody(
  programId:    Program.Id,
  observations: View[ObservationList],
  tileState:    View[ObservationValidationsTableTileState]
) extends ReactFnProps(ObservationValidationsTableBody.component)

object ObservationValidationsTableBody {
  import ValidationsTableRow.*

  private type Props = ObservationValidationsTableBody

  private val ColDef = ColumnDef[Expandable[ValidationsTableRow]]

  private val ObservationIdColumnId     = ColumnId("observation_id")
  private val ObservationTitleColumnId  = ColumnId("observation_title")
  private val ObservationStateColumnId  = ColumnId("observation_state")
  private val ValidationCodeColumnId    = ColumnId("validation_code")
  private val ValidationMessageColumnId = ColumnId("validation_message")
  private val ActionsColumnId           = ColumnId("actions")

  private val columnNames: Map[ColumnId, String] = Map(
    ObservationIdColumnId     -> "Observation Id",
    ObservationTitleColumnId  -> "Title",
    ObservationStateColumnId  -> "State",
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
      import ctx.given

      def obsUrl(obsId: Observation.Id): String    =
        ctx.pageUrl(AppTab.Observations, props.programId, Focused.singleObs(obsId))
      def goToObs(obsId: Observation.Id): Callback =
        ctx.pushPage(AppTab.Observations, props.programId, Focused.singleObs(obsId))

      def toggleAll(row: Row[Expandable[ValidationsTableRow], Nothing]): Callback =
        row.toggleExpanded() *> row.subRows.traverse(r => toggleAll(r)).void

      def requestApprovalButton(row: ValidationsTableRow): Option[Button] =
        row
          .forObsOption(row =>
            val obs = row.obs
            if (obs.hasNeedsApprovalError)
              obs.configuration.flatMap(config =>
                Button(
                  "Request Approval",
                  onClick = props.observations.mod(
                    // this also gets rid of any buttons on "affected" observations
                    _.mapValues(_.id, _.updateToPendingIfConfigurationApplies(config))
                  ) >>
                    ObsQueries.createConfigurationRequest[IO](row.id).void.runAsync
                ).tiny.compact.some
              )
            else none
          )

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
        column(ObservationStateColumnId, _.forObs(_.obs.workflow.state)).setMaxSize(50.toPx),
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
        ),
        ColDef(
          ActionsColumnId,
          cell = cell => requestApprovalButton(cell.row.original.value)
        ).setMaxSize(111.toPx)
      )
    }
    // Rows
    .useMemoBy((props, _, _) => props.observations.get.toList)((_, _, _) =>
      _.filterNot(_.workflow.validationErrors.isEmpty)
        .map(obs =>
          Expandable(
            ObsRow(obs),
            if (obs.workflow.validationErrors.size > 1)
              // only include the tails for messages and validations. The head will be shown in the "parent" row.
              messagesTailRows(obs.id, obs.workflow.validationErrors.head) ++
                obs.workflow.validationErrors.tail
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
    .useEffectOnMountBy((p, _, _, _, table) =>
      val cb = (a: Boolean) => table.toggleAllRowsExpanded(a)
      p.tileState.set(ObservationValidationsTableTileState(cb))
    )
    .useResizeDetector()
    .render((props, _, rows, _, table, resizer) =>
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

  enum ValidationsTableRow {
    case ObsRow(
      obs: Observation
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

    def id: Observation.Id = fold(_.obs.id, _.obsId, _.obsId)

    def forObs[A](f: ObsRow => A): js.UndefOr[A] =
      fold(r => f(r), _ => js.undefined, _ => js.undefined)

    def forObsOption[A](f: ObsRow => Option[A]): Option[A] =
      fold(f, _ => none, _ => none[A])

    def rowId: String =
      fold(
        _.obs.id.toString,
        r => s"${r.obsId}-${r.validation.code.tag}",
        r => s"${r.obsId}-${r.code.tag}-${r.message}"
      )

    private def categoryCell(validations: ObservationValidation*) =
      <.span(validations.map(_.code.name).mkString(", "))
        .withTooltip(content = <.div(validations.toList.toTagMod(ov => <.div(ov.code.description))))

    def category(isExpanded: Boolean): VdomElement =
      fold(
        r =>
          if (isExpanded) categoryCell(r.obs.workflow.validationErrors.head) // head is safe here
          else categoryCell(r.obs.workflow.validationErrors*),
        r => categoryCell(r.validation),
        _ => <.span()
      )

    def message(isExpanded: Boolean): String =
      fold(
        r =>
          if (isExpanded) r.obs.workflow.validationErrors.headOption.map(_.messages.head).orEmpty
          else r.obs.workflow.validationErrors.flatMap(_.messages.toList).mkString(", "),
        r =>
          if (isExpanded) r.validation.messages.head
          else r.validation.messages.mkString_(", "),
        _.message
      )
  }
}

case class ObservationValidationsTableTitle(
  tileState: View[ObservationValidationsTableTileState],
  tileSize:  TileSizeState
) extends ReactFnProps(ObservationValidationsTableTitle.component)

object ObservationValidationsTableTitle:
  private type Props = ObservationValidationsTableTitle

  private val component = ScalaFnComponent[Props]: p =>
    if (p.tileSize === TileSizeState.Minimized)
      EmptyVdom
    else
      <.div(
        ExploreStyles.TableSelectionToolbar,
        Button(
          size = Button.Size.Small,
          icon = Icons.SquarePlus,
          tooltip = "Expand All",
          onClick = p.tileState.get.value(true)
        ).compact,
        Button(
          size = Button.Size.Small,
          icon = Icons.SquareMinus,
          tooltip = "Collapse All",
          onClick = p.tileState.get.value(false)
        ).compact
      )
