// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.Order.*
import cats.syntax.all.*
import crystal.react.*
import crystal.react.hooks.*
import eu.timepit.refined.cats.given
import eu.timepit.refined.types.string.NonEmptyString
import explore.*
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.model.ExploreModelValidators
import explore.model.display.given
import explore.utils.IsExpanded
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.Band
import lucuma.core.math.BrightnessUnits.*
import lucuma.core.math.BrightnessValue
import lucuma.core.math.dimensional.*
import lucuma.core.util.Enumerated
import lucuma.core.util.Of
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Button
import lucuma.react.primereact.Panel
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.refined.*
import lucuma.ui.input.ChangeAuditor
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given
import lucuma.ui.table.*
import lucuma.ui.utils.*
import monocle.Focus

import scala.collection.immutable.SortedMap

sealed trait BrightnessesEditor[T]:
  def brightnesses: View[SortedMap[Band, BrightnessMeasure[T]]]
  def expanded: View[IsExpanded]
  def disabled: Boolean

sealed abstract class BrightnessesEditorBuilder[T, Props <: BrightnessesEditor[T]](implicit
  enumUnits: Enumerated[Units Of Brightness[T]]
):
  protected case class State(usedBands: Set[Band], newBand: Option[Band])

  object State {
    val usedBands = Focus[State](_.usedBands)
    val newBand   = Focus[State](_.newBand)

    def fromUsedBrightnesses(brightnesses: SortedMap[Band, BrightnessMeasure[T]]): State = {
      val usedBands = brightnesses.keySet
      State(usedBands, Band.all.diff(usedBands.toList).headOption)
    }
  }

  protected val label: String // Abstract

  protected def defaultBandUnits: Band => Units Of Brightness[T] // Abstract

  private type RowValue = (Band, View[BrightnessMeasure[T]])

  protected[targeteditor] case class TableMeta(
    // needs to be in the table meta because it is needed by the `delete` column.
    brightnesses: View[SortedMap[Band, BrightnessMeasure[T]]],
    disabled:     Boolean
  )

  private val ColDef = ColumnDef.WithTableMeta[RowValue, TableMeta]

  private val BandColumnId: ColumnId   = ColumnId("band")
  private val ValueColumnId: ColumnId  = ColumnId("value")
  private val UnitsColumnId: ColumnId  = ColumnId("units")
  private val DeleteColumnId: ColumnId = ColumnId("delete")

  protected[targeteditor] val component =
    ScalaFnComponent
      .withHooks[Props]
      .useStateViewBy(props => State.fromUsedBrightnesses(props.brightnesses.get))
      .useEffectWithDepsBy((props, _) => props.brightnesses.get): (_, state) =>
        brightnesses => state.set(State.fromUsedBrightnesses(brightnesses))
      .useMemoBy((_, _) => ()): (props, _) => // cols
        _ =>
          List(
            ColDef(
              BandColumnId,
              _._1,
              "Band",
              _.value.shortName,
              size = 67.toPx
            ).sortable,
            ColDef(
              ValueColumnId,
              _._2.zoom(Measure.valueTagged[BrightnessValue, Brightness[T]]),
              "Value",
              cell =>
                FormInputTextView(
                  id = NonEmptyString.unsafeFrom(s"brightnessValue_${cell.row.id}"),
                  value = cell.value,
                  validFormat = ExploreModelValidators.brightnessValidWedge,
                  changeAuditor =
                    ChangeAuditor.bigDecimal(2.refined, 3.refined).allowExp(2.refined),
                  disabled = cell.table.options.meta.exists(_.disabled)
                ),
              size = 77.toPx
            ).sortableBy(_.get),
            ColDef(
              UnitsColumnId,
              _._2.zoom(Measure.unitsTagged[BrightnessValue, Brightness[T]]),
              "Units",
              cell =>
                EnumDropdownView(
                  id = NonEmptyString.unsafeFrom(s"brightnessUnits_${cell.row.id}"),
                  value = cell.value,
                  disabled = cell.table.options.meta.exists(_.disabled),
                  clazz = ExploreStyles.BrightnessesTableUnitsDropdown
                ),
              size = 145.toPx
            ).sortableBy(_.get),
            ColDef(
              DeleteColumnId,
              _._1,
              "",
              cell =>
                <.div(ExploreStyles.BrightnessesTableDeletButtonWrapper)(
                  Button(
                    icon = Icons.Trash,
                    clazz = ExploreStyles.DeleteButton,
                    text = true,
                    disabled = cell.table.options.meta.exists(_.disabled),
                    onClick = cell.table.options.meta.foldMap(_.brightnesses.mod(_ - cell.value))
                  ).small
                ),
              size = 20.toPx,
              enableSorting = false
            )
          )
      .useMemoBy((props, _, _) => props.brightnesses.get): (props, _, _) => // rows
        _ => props.brightnesses.widen[Map[Band, BrightnessMeasure[T]]].toListOfViews
      .useReactTableBy: (props, _, cols, rows) =>
        TableOptions(
          cols,
          rows,
          getRowId = (row, _, _) => RowId(row._1.tag),
          enableSorting = true,
          enableColumnResizing = true,
          columnResizeMode = ColumnResizeMode.OnChange,
          initialState = TableState(sorting = Sorting(ColumnId("band") -> SortDirection.Ascending)),
          meta = TableMeta(brightnesses = props.brightnesses, disabled = props.disabled)
        )
      .render: (props, state, _, _, table) =>
        val footer =
          <.tr(
            <.td(
              ^.colSpan := 4,
              <.div(
                ExploreStyles.BrightnessesTableFooter,
                state
                  .zoom(State.newBand)
                  .mapValue { (bandView: View[Band]) =>
                    val addBrightness =
                      props.brightnesses.mod(brightnesses =>
                        brightnesses +
                          (bandView.get ->
                            defaultBandUnits(bandView.get)
                              .withValueTagged(BrightnessValue.unsafeFrom(0)))
                      )

                    React.Fragment(
                      EnumDropdownView(
                        id = "NEW_BAND".refined,
                        value = bandView,
                        exclude = state.get.usedBands,
                        clazz = ExploreStyles.FlatFormField, // TODO: Look at this CSS
                        disabled = props.disabled
                      ),
                      Button(
                        icon = Icons.New,
                        onClick = addBrightness,
                        disabled = props.disabled,
                        severity = Button.Severity.Secondary
                      ).mini.compact
                    )
                  }
                  .whenDefined
              )
            )
          )

        Panel(
          header = label,
          toggleable = true,
          onExpand = props.expanded.set(IsExpanded(true)),
          onCollapse = props.expanded.set(IsExpanded(false))
        )(
          <.div(ExploreStyles.ExploreTable |+| ExploreStyles.BrightnessesContainer)(
            PrimeAutoHeightVirtualizedTable(
              table,
              estimateSize = _ => 38.toPx,
              striped = true,
              compact = Compact.Very,
              footerMod = footer,
              tableMod = ExploreStyles.ExploreBorderTable,
              emptyMessage = "No brightnesses defined"
            )
          )
        )

case class IntegratedBrightnessEditor(
  brightnesses: View[SortedMap[Band, BrightnessMeasure[Integrated]]],
  expanded:     View[IsExpanded],
  disabled:     Boolean
) extends ReactFnProps[IntegratedBrightnessEditor](IntegratedBrightnessEditor.component)
    with BrightnessesEditor[Integrated]

object IntegratedBrightnessEditor
    extends BrightnessesEditorBuilder[Integrated, IntegratedBrightnessEditor]:
  protected val label                                                          = "Brightness"
  protected lazy val defaultBandUnits: Band => Units Of Brightness[Integrated] =
    _.defaultIntegrated.units

case class SurfaceBrightnessEditor(
  brightnesses: View[SortedMap[Band, BrightnessMeasure[Surface]]],
  expanded:     View[IsExpanded],
  disabled:     Boolean
) extends ReactFnProps[SurfaceBrightnessEditor](SurfaceBrightnessEditor.component)
    with BrightnessesEditor[Surface]

object SurfaceBrightnessEditor extends BrightnessesEditorBuilder[Surface, SurfaceBrightnessEditor]:
  protected val label                                                       = "Surface Brightness"
  protected lazy val defaultBandUnits: Band => Units Of Brightness[Surface] =
    _.defaultSurface.units
