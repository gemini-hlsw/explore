// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.sequence

import cats.Eq
import cats.syntax.all.*
import explore.*
import explore.components.ui.ExploreStyles
import explore.model.Constants
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.DatasetQaState
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.react.common.ReactFnProps
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.schemas.model.StepRecord
import lucuma.ui.reusability.given
import lucuma.ui.sequence.*
import lucuma.ui.syntax.all.given
import lucuma.ui.table.*

sealed trait VisitTable[D]:
  def steps: List[StepRecord[D]]

  protected[sequence] lazy val rows: List[SequenceRow.Executed.ExecutedStep[D]] =
    steps.map(SequenceRow.Executed.ExecutedStep(_))

case class GmosNorthVisitTable(steps: List[StepRecord[DynamicConfig.GmosNorth]])
    extends ReactFnProps(GmosNorthVisitTable.component)
    with VisitTable[DynamicConfig.GmosNorth]

case class GmosSouthVisitTable(steps: List[StepRecord[DynamicConfig.GmosSouth]])
    extends ReactFnProps(GmosSouthVisitTable.component)
    with VisitTable[DynamicConfig.GmosSouth]

private sealed trait VisitTableBuilder[D: Eq]:
  private type Props = VisitTable[D]

  private case class ExecutedStepRow(step: SequenceRow.Executed.ExecutedStep[D], index: StepIndex)

  private val ColDef = ColumnDef[ExecutedStepRow]

  protected[sequence] val component =
    ScalaFnComponent
      .withHooks[Props]
      .useMemo(()): _ => // cols
        SequenceColumns.gmosColumns(ColDef, _.step.some, _.index.some)
      .useMemoBy((props, _) => props.rows): (_, _) => // rows
        _.zipWithStepIndex.map(ExecutedStepRow.apply)
      .useReactTableBy: (_, cols, rows) =>
        TableOptions(
          cols,
          rows,
          getRowId = (row, _, _) => RowId(row.step.id.toString),
          enableColumnResizing = false,
          enableSorting = false
        )
      .render: (props, _, _, table) =>
        PrimeVirtualizedTable(
          table,
          estimateSize = _ => 28.toPx,
          compact = Compact.Very,
          hoverableRows = true,
          celled = true,
          tableMod = ExploreStyles.SequenceTable,
          renderSubComponent = row =>
            val step = row.original._1
            (<.div(ExploreStyles.VisitStepExtra)(
              <.span(ExploreStyles.VisitStepExtraDatetime)(
                step.startTime.fold("---")(start => Constants.UtcFormatter.format(start))
              ),
              <.span(ExploreStyles.VisitStepExtraDatasets)(
                step.datasets
                  .map(dataset =>
                    <.span(ExploreStyles.VisitStepExtraDatasetItem)(
                      dataset.filename.value,
                      dataset.qaState.map(qaState =>
                        React.Fragment(
                          Icons.Circle.withClass(
                            ExploreStyles.VisitStepExtraDatasetStatusIcon |+|
                              (qaState match
                                case DatasetQaState.Pass   => ExploreStyles.IndicatorOK
                                case DatasetQaState.Usable => ExploreStyles.IndicatorWarning
                                case DatasetQaState.Fail   => ExploreStyles.IndicatorFail
                              )
                          ),
                          <.span(ExploreStyles.VisitStepExtraDatasetStatusLabel)(
                            qaState.shortName
                          )
                        )
                      )
                    )
                  )
                  .toVdomArray
              )
            ): VdomNode).some
        )

object GmosNorthVisitTable extends VisitTableBuilder[DynamicConfig.GmosNorth]

object GmosSouthVisitTable extends VisitTableBuilder[DynamicConfig.GmosSouth]
