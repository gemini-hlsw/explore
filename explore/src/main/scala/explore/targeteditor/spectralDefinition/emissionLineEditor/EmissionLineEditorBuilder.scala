// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor.spectralDefinition.emissionLineEditor

import cats.Order.*
import cats.syntax.all.*
import coulomb.*
import coulomb.syntax.*
import crystal.react.*
import crystal.react.hooks.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.*
import explore.components.ui.ExploreStyles
import explore.config.ConfigurationFormats.*
import explore.model.enums.WavelengthUnits
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

private abstract class EmissionLineEditorBuilder[T, Props <: EmissionLineEditor[T]](using
  enumUnits: Enumerated[Units Of LineFlux[T]]
):
  protected val defaultLineUnits: Units Of LineFlux[T]

  private type RowValue = (Wavelength, View[EmissionLine[T]])

  protected[targeteditor] case class TableMeta(
    disabled:         Boolean,
    emissionLinesMod: (
      SortedMap[Wavelength, EmissionLine[T]] => SortedMap[Wavelength, EmissionLine[T]]
    ) => Callback
  )

  private val ColDef = ColumnDef[RowValue].WithTableMeta[TableMeta]

  private val WavelengthColumnId: ColumnId = ColumnId("wavelength")
  private val LineValueColumnId: ColumnId  = ColumnId("lineValue")
  private val LineUnitsColumnId: ColumnId  = ColumnId("lineUnits")
  private val DeleteColumnId: ColumnId     = ColumnId("delete")

  private def columnsWithWavelengthUnits(wavelengthUnits: WavelengthUnits) =
    Reusable.always:
      List(
        ColDef(
          WavelengthColumnId,
          _._1,
          _ => <.span(ExploreStyles.TextPlain, s"λ (${wavelengthUnits.symbol})"),
          cell => wavelengthUnits.format.reverseGet(cell.value),
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
                .andThen(LineWidthValue.Value.reverse),
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
                .andThen(LineFluxValue.Value.reverse),
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
                onClick = cell.table.options.meta.map(_.emissionLinesMod(_ - cell.value)).orEmpty
              ).small
            ),
          size = 20.toPx,
          enableSorting = false
        )
      )

  protected[targeteditor] def componentWithWavelengthUnits(wavelengthUnits: WavelengthUnits) =
    ScalaFnComponent[Props]: props =>
      for
        rows          <- useMemo(props.emissionLines.get): _ =>
                           props.emissionLines.widen[Map[Wavelength, EmissionLine[T]]].toListOfViews
        table         <- useReactTable:
                           TableOptions(
                             columnsWithWavelengthUnits(wavelengthUnits),
                             rows,
                             getRowId = (row, _, _) => RowId(row._1.toPicometers.value.toString),
                             enableSorting = true,
                             enableColumnResizing = true,
                             columnResizeMode = ColumnResizeMode.OnChange,
                             initialState = TableState(sorting =
                               Sorting(ColumnId("wavelength") -> SortDirection.Ascending)
                             ),
                             meta = TableMeta(
                               disabled = props.disabled,
                               emissionLinesMod = props.emissionLines.mod
                             )
                           )
        newWavelength <- useStateView(none[Wavelength])
        addDisabled   <- useStateView(AddDisabled(true))
      yield
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
              validFormat = wavelengthUnits.toInputWedge,
              changeAuditor = wavelengthUnits.toAuditor,
              onTextChange = (s: String) => addDisabled.set(AddDisabled(s.isEmpty)),
              units = wavelengthUnits.symbol
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
