// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.sequence

import cats.syntax.all.*
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.SizePx
import lucuma.react.resizeDetector.hooks.*
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.schemas.model.enums.StepExecutionState
import lucuma.ui.reusability.given
import lucuma.ui.sequence.*
import lucuma.ui.syntax.all.given
import lucuma.ui.table.*
import lucuma.ui.table.ColumnSize.*
import lucuma.ui.table.hooks.*

import scala.scalajs.LinkingInfo

private type SequenceColumnsType[D] =
  SequenceColumns[D, SequenceIndexedRow[D], SequenceRow[D], Nothing, Nothing, Nothing]
private type ColumnType[D]          =
  ColumnDef[Expandable[HeaderOrRow[SequenceIndexedRow[D]]], ?, Nothing, Nothing, Nothing, Any, Any]

private trait SequenceTableBuilder[S, D](
  instrumentColumns: SequenceColumnsType[D] => List[ColumnType[D]]
) extends SequenceRowBuilder[D]:
  private type Props = SequenceTable[S, D]

  private lazy val ColDef = ColumnDef[SequenceTableRowType]

  private val HeaderColumnId: ColumnId   = ColumnId("header")
  private val ExtraRowColumnId: ColumnId = ColumnId("extraRow")

  private lazy val ColumnSizes: Map[ColumnId, ColumnSize] = Map(
    HeaderColumnId   -> FixedSize(0.toPx),
    ExtraRowColumnId -> FixedSize(0.toPx)
  ) ++ SequenceColumns.BaseColumnSizes

  private lazy val columns: Reusable[List[ColDef.Type]] =
    Reusable.always:
      List(
        SequenceColumns
          .headerCell(HeaderColumnId, ColDef)
          .withColumnSize(ColumnSizes(HeaderColumnId)),
        ColDef(
          ExtraRowColumnId,
          header = "",
          cell = _.row.original.value.toOption
            .map(_.step)
            .collect:
              case step @ SequenceRow.Executed.ExecutedStep(_, _) =>
                renderVisitExtraRow(step, showOngoingLabel = true)
        ).withColumnSize(ColumnSizes(ExtraRowColumnId))
      ) ++ instrumentColumns(SequenceColumns(ColDef, _.step.some, _.index.some))

  private lazy val DynTableDef = DynTable(
    ColumnSizes,
    SequenceColumns.BaseColumnPriorities,
    DynTable.ColState(
      resized = ColumnSizing(),
      visibility = ColumnVisibility()
    )
  )

  protected[sequence] val component =
    ScalaFnComponent[Props]: props =>
      for
        ctx        <- useContext(AppContext.ctx)
        visitsData <- useMemo(props.visits):
                        visitsSequences(_, none)
        rows       <-
          useMemo(
            (visitsData, props.acquisitionRows, props.scienceRows, props.currentVisitId)
          ): (visitsData, acquisitionRows, scienceRows, currentVisitId) =>
            val (visitRows, nextScienceIndex): (List[VisitData], StepIndex) = visitsData.value
            stitchSequence(
              visitRows,
              currentVisitId,
              nextScienceIndex,
              acquisitionRows,
              scienceRows
            )
        resize     <- useResizeDetector
        dynTable   <- useDynTable(DynTableDef, SizePx(resize.width.orEmpty))
        table      <-
          useReactTable:
            TableOptions(
              columns.map(dynTable.setInitialColWidths),
              rows,
              enableSorting = false,
              enableColumnResizing = true,
              enableExpanding = true,
              getRowId = (row, _, _) => getRowId(row),
              getSubRows = (row, _) => row.subRows,
              columnResizeMode = ColumnResizeMode.OnChange,
              initialState = TableState(
                expanded = CurrentExpandedState
              ),
              state = PartialTableState(
                columnSizing = dynTable.columnSizing,
                columnVisibility = dynTable.columnVisibility
              ),
              onColumnSizingChange = dynTable.onColumnSizingChangeHandler
            )
      yield
        val extraRowMod: TagMod =
          TagMod(
            SequenceStyles.ExtraRowShown,
            resize.width
              .map: w =>
                ^.width := s"${w}px"
              .whenDefined
          )

        PrimeAutoHeightVirtualizedTable(
          table,
          estimateSize = index =>
            table.getRowModel().rows.get(index).map(_.original.value) match
              case Some(Right(SequenceIndexedRow(SequenceRow.Executed.ExecutedStep(_, _), _))) =>
                SequenceRowHeight.WithExtra
              case _                                                                           =>
                SequenceRowHeight.Regular,
          overscan = 8,
          containerRef = resize.ref,
          compact = Compact.Very,
          hoverableRows = true,
          celled = true,
          tableMod = SequenceStyles.SequenceTable,
          headerCellMod = _.column.id match
            case id if id == HeaderColumnId   => SequenceStyles.HiddenColTableHeader
            case id if id == ExtraRowColumnId => SequenceStyles.HiddenColTableHeader
            case _                            => TagMod.empty,
          rowMod = _.original.value.fold(
            _ => ExploreStyles.SequenceRowHeader,
            stepRow =>
              val step: SequenceRow[D] = stepRow.step
              TagMod(
                step match
                  case SequenceRow.Executed.ExecutedStep(step, _)                    =>
                    SequenceStyles.RowHasExtra |+|
                      ExploreStyles.SequenceRowDone.unless_(
                        step.executionState == StepExecutionState.Ongoing
                      )
                  case SequenceRow.FutureStep(_, _, firstOf, _) if firstOf.isDefined =>
                    ExploreStyles.SequenceRowFirstInAtom
                  case _                                                             => TagMod.empty,
                if (LinkingInfo.developmentMode)
                  step.id.toOption.map(^.title := _.toString).whenDefined
                else TagMod.empty
              )
          ),
          cellMod = cell =>
            cell.row.original.value match
              case Left(_)        => // Header
                cell.column.id match
                  case id if id == HeaderColumnId => TagMod(^.colSpan := columns.length)
                  case _                          => ^.display.none
              case Right(stepRow) =>
                cell.column.id match
                  case id if id == ExtraRowColumnId =>
                    stepRow.step match // Extra row is shown in a selected row or in an executed step row.
                      case SequenceRow.Executed.ExecutedStep(_, _) => extraRowMod
                      case _                                       => TagMod.empty
                  case _                            =>
                    TagMod.empty
        )
