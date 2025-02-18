// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.sequence

import cats.Eq
import cats.effect.IO
import cats.syntax.all.*
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.all.svg.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.SequenceType
import lucuma.core.math.SignalToNoise
import lucuma.core.model.sequence.*
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.core.model.sequence.gmos.StaticConfig
import lucuma.react.SizePx
import lucuma.react.common.ReactFnProps
import lucuma.react.resizeDetector.hooks.*
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.schemas.model.Visit
import lucuma.schemas.model.enums.AtomExecutionState
import lucuma.ui.reusability.given
import lucuma.ui.sequence.*
import lucuma.ui.syntax.all.given
import lucuma.ui.table.*
import lucuma.ui.table.ColumnSize.*
import lucuma.ui.table.hooks.*
import org.http4s.client.Client
import org.typelevel.log4cats.Logger

import scala.scalajs.LinkingInfo

sealed trait GmosSequenceTable[S, D]:
  def visits: List[Visit[D]]
  def config: ExecutionConfig[S, D]
  def snPerClass: Map[ObserveClass, SignalToNoise]

  private def futureSteps(
    obsClass:      ObserveClass,
    currentStepId: Option[Step.Id] // Will be shown in visits.
  )(sequence: ExecutionSequence[D]): List[SequenceRow.FutureStep[D]] =
    SequenceRow.FutureStep
      .fromAtoms(
        sequence.nextAtom +: (
          obsClass match // For acquisition, we ignore possibleFuture
            case ObserveClass.Science => sequence.possibleFuture
            case _                    => List.empty
        ),
        i => // Only show S/N for science or acq if FPU is None
          snPerClass
            .get(obsClass)
            .filter: _ =>
              i.observeClass match
                case a @ ObserveClass.Acquisition =>
                  i.instrumentConfig match
                    case DynamicConfig.GmosNorth(_, _, _, _, _, _, None) => true
                    case DynamicConfig.GmosSouth(_, _, _, _, _, _, None) => true
                    case _                                               => false
                case ObserveClass.Science         => true
                case _                            => false
      )
      .filterNot(futureStep => currentStepId.contains_(futureStep.stepId))

  private lazy val currentVisitData: Option[(Visit.Id, SequenceType, Option[Step.Id])] =
    // If the last atom of the last visit is Ongoing, the sequence is executing.
    visits.lastOption
      .filter:
        _.atoms.lastOption.exists:
          _.executionState === AtomExecutionState.Ongoing
      // We omit the Ongoing step from the visits.
      .map(visit =>
        (visit.id,
         visit.atoms.last.sequenceType,
         visit.atoms.lastOption
           .flatMap(_.steps.lastOption)
           .flatMap(_.generatedId)
        )
      )

  protected[sequence] lazy val currentVisitId: Option[Visit.Id]              =
    currentVisitData.map(_._1)
  protected[sequence] lazy val currentAtomSequenceType: Option[SequenceType] =
    currentVisitData.map(_._2)
  protected[sequence] lazy val currentStepId: Option[Step.Id]                =
    currentVisitData.flatMap(_._3)

  protected[sequence] lazy val scienceRows: List[SequenceRow[D]] =
    config.science
      .map(futureSteps(ObserveClass.Science, currentStepId))
      .orEmpty

  // Hide acquisition when science is executing or when sequence is complete.
  protected[sequence] lazy val isAcquisitionDisplayed: Boolean =
    !currentAtomSequenceType.contains_(SequenceType.Science) && scienceRows.nonEmpty

  protected[sequence] lazy val acquisitionRows: List[SequenceRow[D]] =
    config.acquisition // If we are executing Science, don't show any future acquisition rows.
      .filter(_ => isAcquisitionDisplayed)
      .map(futureSteps(ObserveClass.Acquisition, currentStepId))
      .orEmpty

case class GmosNorthSequenceTable(
  visits:     List[Visit.GmosNorth],
  config:     ExecutionConfig[StaticConfig.GmosNorth, DynamicConfig.GmosNorth],
  snPerClass: Map[ObserveClass, SignalToNoise]
) extends ReactFnProps(GmosNorthSequenceTable.component)
    with GmosSequenceTable[StaticConfig.GmosNorth, DynamicConfig.GmosNorth]

case class GmosSouthSequenceTable(
  visits:     List[Visit.GmosSouth],
  config:     ExecutionConfig[StaticConfig.GmosSouth, DynamicConfig.GmosSouth],
  snPerClass: Map[ObserveClass, SignalToNoise]
) extends ReactFnProps(GmosSouthSequenceTable.component)
    with GmosSequenceTable[StaticConfig.GmosSouth, DynamicConfig.GmosSouth]

private sealed trait GmosSequenceTableBuilder[S, D: Eq] extends SequenceRowBuilder[D]:
  private type Props = GmosSequenceTable[S, D]

  private lazy val ColDef = ColumnDef[SequenceTableRowType]

  private def drawBracket(rows: Int): VdomElement =
    svg(^.width := "1px", ^.height := "15px")(
      use(
        transform := s"scale(1, ${math.pow(rows.toDouble, 0.9)})",
        xlinkHref := "/bracket.svg#bracket"
      )
    )

  private val HeaderColumnId: ColumnId    = ColumnId("header")
  private val ExtraRowColumnId: ColumnId  = ColumnId("extraRow")
  private val AtomStepsColumnId: ColumnId = ColumnId("atomSteps")

  private lazy val ColumnSizes: Map[ColumnId, ColumnSize] = Map(
    HeaderColumnId    -> FixedSize(0.toPx),
    ExtraRowColumnId  -> FixedSize(0.toPx),
    AtomStepsColumnId -> FixedSize(30.toPx)
  ) ++ SequenceColumns.BaseColumnSizes

  private def columns(httpClient: Client[IO])(using
    Logger[IO]
  ): List[ColumnDef.NoMeta[SequenceTableRowType, ?]] =
    List(
      SequenceColumns.headerCell(HeaderColumnId, ColDef).setColumnSize(ColumnSizes(HeaderColumnId)),
      ColDef(
        AtomStepsColumnId,
        _.value.toOption
          .map(_.step)
          .collect:
            case SequenceRow.FutureStep(_, _, firstOf, _) => firstOf,
        header = " ",
        cell = _.value.flatMap(_.map(drawBracket))
      ).setColumnSize(ColumnSizes(HeaderColumnId)),
      ColDef(
        ExtraRowColumnId,
        header = "",
        cell = _.row.original.value.toOption
          .map(_.step)
          .collect:
            case step @ SequenceRow.Executed.ExecutedStep(_, _) =>
              renderVisitExtraRow(httpClient)(step)
      ).setColumnSize(ColumnSizes(ExtraRowColumnId))
    ) ++ SequenceColumns.gmosColumns(ColDef, _.step.some, _.index.some)

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
        cols       <- useMemo(()): _ =>
                        import ctx.given
                        columns(ctx.httpClient)
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
        table      <- useReactTable:
                        TableOptions(
                          cols.map(dynTable.setInitialColWidths),
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
                ^.width := s"${w - ColumnSizes(AtomStepsColumnId).initial.value}px"
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
          rowMod = _.original.value.toOption
            .map(_.step)
            .map: s =>
              TagMod(
                s match
                  case SequenceRow.Executed.ExecutedStep(_, _) =>
                    SequenceStyles.RowHasExtra |+| ExploreStyles.SequenceRowDone
                  case _                                       => TagMod.empty,
                if (LinkingInfo.developmentMode)
                  s.id.toOption.map(^.title := _.toString).whenDefined
                else TagMod.empty
              )
            .whenDefined,
          cellMod = cell =>
            cell.row.original.value match
              case Left(_)        => // Header
                cell.column.id match
                  case id if id == HeaderColumnId => TagMod(^.colSpan := cols.length)
                  case _                          => ^.display.none
              case Right(stepRow) =>
                cell.column.id match
                  case id if id == ExtraRowColumnId                           =>
                    stepRow.step match // Extra row is shown in a selected row or in an executed step row.
                      case SequenceRow.Executed.ExecutedStep(_, _) => extraRowMod
                      case _                                       => TagMod.empty
                  case colId if colId == AtomStepsColumnId                    =>
                    ExploreStyles.SequenceBracketCell
                  case colId if colId == SequenceColumns.IndexAndTypeColumnId =>
                    ExploreStyles.CellHideBorder // Hide border between bracket column and next one
                  case _ =>
                    TagMod.empty
        )

object GmosNorthSequenceTable
    extends GmosSequenceTableBuilder[StaticConfig.GmosNorth, DynamicConfig.GmosNorth]

object GmosSouthSequenceTable
    extends GmosSequenceTableBuilder[StaticConfig.GmosSouth, DynamicConfig.GmosSouth]
