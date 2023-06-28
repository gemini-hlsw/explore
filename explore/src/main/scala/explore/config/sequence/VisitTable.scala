// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config.sequence

import cats.syntax.all.*
import crystal.react.*
import crystal.react.hooks.*
import explore.*
import explore.components.ui.ExploreStyles
import explore.model.Constants
import explore.model.reusability.given
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.DatasetQaState
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.schemas.model.StepRecord
import lucuma.ui.syntax.all.given
import lucuma.ui.table.*
import react.common.ReactFnProps

final case class VisitTable(steps: List[StepRecord]) extends ReactFnProps(VisitTable.component)

object VisitTable:
  private type Props = VisitTable

  private val ColDef = ColumnDef[(GmosSequenceRow.Executed.ExecutedStep, Int)]

  val IndexColumnId: ColumnId = ColumnId("stepIndex")

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useMemo(())(_ =>
        ColDef(IndexColumnId, _._2 + 1, header = "Step", cell = _.value.toString, size = 30.toPx) +:
          SequenceColumns.gmosColumns(ColDef, _._1.some)
      )
      .useMemoBy((props, _) => props.steps)((_, _) =>
        _.map(step => GmosSequenceRow.Executed.ExecutedStep(step))
      )
      .useReactTableBy((_, cols, rows) =>
        TableOptions(
          cols,
          rows.map(_.zipWithIndex),
          getRowId = (row, _, _) => RowId(row._1.id.toString),
          enableColumnResizing = false,
          enableSorting = false
        )
      )
      .render((props, _, _, table) =>
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
      )
