// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.Order.*
import cats.syntax.all.*
import coulomb.*
import coulomb.syntax.*
import crystal.react.View
import crystal.react.hooks.*
import crystal.react.implicits.*
import crystal.react.reuse.*
import eu.timepit.refined.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.types.numeric.PosBigDecimal
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.*
import explore.components.ui.ExploreStyles
import explore.model.enums.*
import explore.model.formats.*
import explore.utils.IsExpanded
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.math.BrightnessUnits.*
import lucuma.core.math.Wavelength
import lucuma.core.math.dimensional.*
import lucuma.core.math.units.*
import lucuma.core.model.EmissionLine
import lucuma.core.util.Enumerated
import lucuma.core.util.Of
import lucuma.core.validation.*
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.refined.*
import lucuma.ui.input.ChangeAuditor
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import lucuma.ui.table.*
import lucuma.ui.utils.*
import react.common.ReactFnProps
import react.primereact.Button
import react.primereact.Panel
import reactST.{tanstackTableCore => raw}

import scala.collection.immutable.SortedMap
import scala.math.BigDecimal.RoundingMode

import scalajs.js.JSConverters.*

sealed trait EmissionLineEditor[T]:
  def emissionLines: View[SortedMap[Wavelength, EmissionLine[T]]]
  def expanded: View[IsExpanded]
  def disabled: Boolean

sealed abstract class EmissionLineEditorBuilder[T, Props <: EmissionLineEditor[T]](using
  enumUnits: Enumerated[Units Of LineFlux[T]]
):
  protected val defaultLineUnits: Units Of LineFlux[T]

  private type RowValue = (Wavelength, View[EmissionLine[T]])

  private val ColDef = ColumnDef[RowValue]

  private val WavelengthColumnId: ColumnId = ColumnId("wavelength")
  private val LineValueColumnId: ColumnId  = ColumnId("lineValue")
  private val LineUnitsColumnId: ColumnId  = ColumnId("lineUnits")
  private val DeleteColumnId: ColumnId     = ColumnId("delete")

  val component = ScalaFnComponent
    .withHooks[Props]
    // Memo cols
    .useMemoBy(props => (props.emissionLines.reuseByValue, props.disabled)) {
      _ => (emissionLines, disabled) =>
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
                validFormat = InputValidSplitEpi.posBigDecimal,
                changeAuditor = ChangeAuditor.posBigDecimal(3.refined).allowEmpty,
                disabled = disabled
              ),
            size = 116.toPx
          ),
          ColDef(
            LineValueColumnId,
            _._2.zoom(
              EmissionLine.lineFlux.andThen(Measure.valueTagged[PosBigDecimal, LineFlux[T]])
            ),
            "Brightness",
            cell =>
              FormInputTextView(
                id = NonEmptyString.unsafeFrom(s"lineValue_${cell.row.id}"),
                value = cell.value,
                validFormat = InputValidSplitEpi.posBigDecimalWithScientificNotation,
                changeAuditor = ChangeAuditor.posScientificNotation(),
                disabled = disabled
              ),
            size = 102.toPx
          ),
          ColDef(
            LineUnitsColumnId,
            _._2.zoom(
              EmissionLine.lineFlux.andThen(Measure.unitsTagged[PosBigDecimal, LineFlux[T]])
            ),
            "Units",
            cell =>
              EnumDropdownView(
                id = NonEmptyString.unsafeFrom(s"lineUnits_${cell.row.id}"),
                value = cell.value,
                disabled = disabled,
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
                  disabled = disabled,
                  onClick = emissionLines.mod(_ - cell.value)
                ).small
              ),
            size = 20.toPx,
            enableSorting = false
          )
        )
    }
    // rows
    .useMemoBy((props, _) => props.emissionLines.get)((props, _) =>
      _ => props.emissionLines.widen[Map[Wavelength, EmissionLine[T]]].toListOfViews
    )
    .useReactTableBy((_, cols, rows) =>
      TableOptions(
        cols,
        rows,
        getRowId = (row, _, _) => RowId(row._1.toPicometers.value.toString),
        enableSorting = true,
        enableColumnResizing = true,
        columnResizeMode = ColumnResizeMode.OnChange,
        initialState =
          TableState(sorting = Sorting(ColumnId("wavelength") -> SortDirection.Ascending))
      )
    )
    // newWavelength
    .useStateView(none[Wavelength])
    // addDisabled
    .useStateView(AddDisabled(true))
    .render { (props, _, _, table, newWavelength, addDisabled) =>
      val bd1 = PosBigDecimal.unsafeFrom(BigDecimal(1))

      val addLine =
        newWavelength.get.foldMap(wavelength =>
          props.emissionLines.mod(emissionLines =>
            emissionLines +
              (wavelength -> EmissionLine(
                bd1.withUnit[KilometersPerSecond],
                defaultLineUnits.withValueTagged(bd1)
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
    }

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
