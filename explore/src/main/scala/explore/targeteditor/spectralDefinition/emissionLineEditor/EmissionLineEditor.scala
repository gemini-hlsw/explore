// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor.spectralDefinition.emissionLineEditor

import cats.Order.*
import cats.syntax.all.*
import coulomb.*
import coulomb.syntax.*
import crystal.react.*
import crystal.react.hooks.*
import eu.timepit.refined.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.*
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.model.formats.*
import explore.utils.IsExpanded
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.math.BrightnessUnits.*
import lucuma.core.math.LineFluxValue
import lucuma.core.math.LineFluxValueRefinement
import lucuma.core.math.LineWidthValue
import lucuma.core.math.LineWidthValueRefinement
import lucuma.core.math.Wavelength
import lucuma.core.math.dimensional.*
import lucuma.core.math.units.*
import lucuma.core.model.EmissionLine
import lucuma.core.util.Enumerated
import lucuma.core.util.Of
import lucuma.core.validation.*
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

import scala.collection.immutable.SortedMap
import scala.math.BigDecimal.RoundingMode

sealed trait EmissionLineEditor[T]:
  def emissionLines: View[SortedMap[Wavelength, EmissionLine[T]]]
  def expanded: View[IsExpanded]
  def disabled: Boolean

sealed abstract class EmissionLineEditorBuilder[T, Props <: EmissionLineEditor[T]](using
  enumUnits: Enumerated[Units Of LineFlux[T]]
):
  protected val defaultLineUnits: Units Of LineFlux[T]

  private type RowValue = (Wavelength, View[EmissionLine[T]])

  protected[targeteditor] case class TableMeta(disabled: Boolean)

  private val ColDef = ColumnDef.WithTableMeta[RowValue, TableMeta]

  private val WavelengthColumnId: ColumnId = ColumnId("wavelength")
  private val LineValueColumnId: ColumnId  = ColumnId("lineValue")
  private val LineUnitsColumnId: ColumnId  = ColumnId("lineUnits")
  private val DeleteColumnId: ColumnId     = ColumnId("delete")

  protected[targeteditor] val component = ScalaFnComponent
    .withHooks[Props]
    .useMemoBy(_ => ()): props => // cols
      _ =>
        List(
          ColDef(
            WavelengthColumnId,
            _._1,
            _ => <.span(ExploreStyles.TextPlain, "λ (µm)"),
            cell =>
              Wavelength.decimalMicrometers
                .reverseGet(cell.value)
                .setScale(3, RoundingMode.HALF_UP)
                .toString,
            size = 74.toPx
          ).sortable,
          ColDef(
            ColumnId("width"),
            _._2.zoom(EmissionLine.lineWidth[T]).stripQuantity,
            "Width (km/s)",
            cell =>
              FormInputTextView(
                id = NonEmptyString.unsafeFrom(s"lineWidth_${cell.row.id}"),
                value = cell.value,
                validFormat = InputValidSplitEpi
                  .refinedBigDecimal[LineWidthValueRefinement]
                  .andThen(LineWidthValue.value.reverse),
                changeAuditor = ChangeAuditor.posBigDecimal(3.refined).allowEmpty,
                disabled = cell.table.options.meta.exists(_.disabled)
              ),
            size = 116.toPx
          ),
          ColDef(
            LineValueColumnId,
            _._2.zoom(EmissionLine.lineFlux.andThen(Measure.valueTagged)),
            "Brightness",
            cell =>
              FormInputTextView(
                id = NonEmptyString.unsafeFrom(s"lineValue_${cell.row.id}"),
                value = cell.value,
                validFormat = InputValidSplitEpi
                  .refinedBigDecimalWithScientificNotation[LineFluxValueRefinement]
                  .andThen(LineFluxValue.value.reverse),
                changeAuditor = ChangeAuditor.posScientificNotation(),
                disabled = cell.table.options.meta.exists(_.disabled)
              ),
            size = 102.toPx
          ),
          ColDef(
            LineUnitsColumnId,
            _._2.zoom(EmissionLine.lineFlux.andThen(Measure.unitsTagged)),
            "Units",
            cell =>
              EnumDropdownView(
                id = NonEmptyString.unsafeFrom(s"lineUnits_${cell.row.id}"),
                value = cell.value,
                disabled = cell.table.options.meta.exists(_.disabled),
                clazz = ExploreStyles.BrightnessesTableUnitsDropdown
              ),
            size = 171.toPx
          ),
          ColDef(
            DeleteColumnId,
            _._1,
            "",
            cell =>
              <.div(
                ExploreStyles.BrightnessesTableDeletButtonWrapper,
                Button(
                  icon = Icons.Trash,
                  clazz = ExploreStyles.DeleteButton,
                  text = true,
                  disabled = cell.table.options.meta.exists(_.disabled),
                  onClick = props.emissionLines.mod(_ - cell.value)
                ).small
              ),
            size = 20.toPx,
            enableSorting = false
          )
        )
    .useMemoBy((props, _) => props.emissionLines.get): (props, _) => // rows
      _ => props.emissionLines.widen[Map[Wavelength, EmissionLine[T]]].toListOfViews
    .useReactTableBy: (props, cols, rows) =>
      TableOptions(
        cols,
        rows,
        getRowId = (row, _, _) => RowId(row._1.toPicometers.value.toString),
        enableSorting = true,
        enableColumnResizing = true,
        columnResizeMode = ColumnResizeMode.OnChange,
        initialState =
          TableState(sorting = Sorting(ColumnId("wavelength") -> SortDirection.Ascending)),
        meta = TableMeta(disabled = props.disabled)
      )
    .useStateView(none[Wavelength]) // newWavelength
    .useStateView(AddDisabled(true)) // addDisabled
    .render: (props, _, _, table, newWavelength, addDisabled) =>
      val addLine =
        newWavelength.get.foldMap(wavelength =>
          props.emissionLines.mod(emissionLines =>
            emissionLines +
              (wavelength -> EmissionLine(
                LineWidthValue.unsafeFrom(1).withUnit[KilometersPerSecond],
                defaultLineUnits.withValueTagged(LineFluxValue.unsafeFrom(1))
              ))
          ) >> newWavelength.set(none)
        )

      val footer =
        <.div(
          ExploreStyles.BrightnessesTableFooter,
          FormInputTextView(
            id = "newWavelength".refined,
            value = newWavelength,
            label = "New line λ:",
            validFormat = InputValidSplitEpi.fromFormat(formatWavelengthMicron).optional,
            changeAuditor = ChangeAuditor
              .fromFormat(formatWavelengthMicron)
              .decimal(3.refined)
              .allow(List("0", "0.").contains)
              .optional,
            onTextChange = (s: String) => addDisabled.set(AddDisabled(s.isEmpty)),
            units = "μm"
          ),
          Button(
            icon = Icons.New,
            onClick = addLine,
            severity = Button.Severity.Secondary,
            disabled = props.disabled || addDisabled.get.value
          ).mini.compact
        )

      Panel(
        header = "Brightness",
        toggleable = true,
        onExpand = props.expanded.set(IsExpanded(true)),
        onCollapse = props.expanded.set(IsExpanded(false))
      )(
        <.div(ExploreStyles.ExploreTable |+| ExploreStyles.BrightnessesContainer)(
          PrimeAutoHeightVirtualizedTable(
            table,
            estimateSize = _ => 34.toPx,
            striped = true,
            compact = Compact.Very,
            tableMod = ExploreStyles.ExploreBorderTable,
            emptyMessage = "No lines defined"
          ),
          footer
        )
      )

case class IntegratedEmissionLineEditor(
  emissionLines: View[SortedMap[Wavelength, EmissionLine[Integrated]]],
  expanded:      View[IsExpanded],
  disabled:      Boolean
) extends ReactFnProps[IntegratedEmissionLineEditor](IntegratedEmissionLineEditor.component)
    with EmissionLineEditor[Integrated]

object IntegratedEmissionLineEditor
    extends EmissionLineEditorBuilder[Integrated, IntegratedEmissionLineEditor] {
  val defaultLineUnits =
    summon[TaggedUnit[ErgsPerSecondCentimeter2, LineFlux[Integrated]]].unit
}

case class SurfaceEmissionLineEditor(
  emissionLines: View[SortedMap[Wavelength, EmissionLine[Surface]]],
  expanded:      View[IsExpanded],
  disabled:      Boolean
) extends ReactFnProps[SurfaceEmissionLineEditor](SurfaceEmissionLineEditor.component)
    with EmissionLineEditor[Surface]

object SurfaceEmissionLineEditor
    extends EmissionLineEditorBuilder[Surface, SurfaceEmissionLineEditor] {

  val defaultLineUnits =
    summon[TaggedUnit[ErgsPerSecondCentimeter2Arcsec2, LineFlux[Surface]]].unit
}
