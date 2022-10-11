// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
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
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.math.BrightnessUnits.*
import lucuma.core.math.Wavelength
import lucuma.core.math.dimensional.*
import lucuma.core.math.units.*
import lucuma.core.model.EmissionLine
import lucuma.core.util.Enumerated
import lucuma.core.validation.*
import lucuma.react.table.*
import lucuma.refined.*
import lucuma.ui.forms.EnumViewSelect
import lucuma.ui.forms.FormInputEV
import lucuma.ui.input.ChangeAuditor
import lucuma.ui.reusability.*
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import lucuma.ui.table.*
import react.common.ReactFnProps
import react.semanticui.collections.table.*
import react.semanticui.elements.button.Button
import react.semanticui.sizes.*
import reactST.{tanstackTableCore => raw}

import scala.collection.immutable.SortedMap
import scala.math.BigDecimal.RoundingMode

import scalajs.js.JSConverters.*

sealed trait EmissionLineEditor[T] {
  val emissionLines: View[SortedMap[Wavelength, EmissionLine[T]]]
  val disabled: Boolean
}

sealed abstract class EmissionLineEditorBuilder[T, Props <: EmissionLineEditor[T]](using
  enumUnits: Enumerated[Units Of LineFlux[T]]
):
  protected val defaultLineUnits: Units Of LineFlux[T]

  private type RowValue = (Wavelength, View[EmissionLine[T]])

  private val ColDef = ColumnDef[RowValue]

  val component = ScalaFnComponent
    .withHooks[Props]
    // Memo cols
    .useMemoBy(props => (props.emissionLines.reuseByValue, props.disabled)) {
      _ => (emissionLines, disabled) =>
        List(
          ColDef(
            "wavelength",
            _._1,
            _ => <.span(ExploreStyles.TextPlain, "λ (µm)"),
            cell =>
              Wavelength.decimalMicrometers
                .reverseGet(cell.value)
                .setScale(3, RoundingMode.HALF_UP)
                .toString,
            size = 60,
            minSize = 50,
            maxSize = 80
          ).sortable,
          ColDef(
            "width",
            _._2.zoom(EmissionLine.lineWidth[T]).stripQuantity,
            "Width (km/s)",
            cell =>
              FormInputEV[View, PosBigDecimal](
                id = NonEmptyString.unsafeFrom(s"lineWidth_${cell.row.id}"),
                value = cell.value,
                validFormat = InputValidSplitEpi.posBigDecimal,
                changeAuditor = ChangeAuditor.posBigDecimal(3.refined).allowEmpty,
                disabled = disabled
              ),
            size = 100,
            minSize = 80,
            maxSize = 160
          ),
          ColDef(
            "lineValue",
            _._2.zoom(
              EmissionLine.lineFlux.andThen(Measure.valueTagged[PosBigDecimal, LineFlux[T]])
            ),
            "Brightness",
            cell =>
              FormInputEV[View, PosBigDecimal](
                id = NonEmptyString.unsafeFrom(s"lineValue_${cell.row.id}"),
                value = cell.value,
                validFormat = InputValidSplitEpi.posBigDecimalWithScientificNotation,
                changeAuditor = ChangeAuditor.posScientificNotation(),
                disabled = disabled
              ),
            size = 80,
            minSize = 70,
            maxSize = 160
          ),
          ColDef(
            "lineUnits",
            _._2.zoom(
              EmissionLine.lineFlux.andThen(Measure.unitsTagged[PosBigDecimal, LineFlux[T]])
            ),
            "Units",
            cell =>
              EnumViewSelect[View, Units Of LineFlux[T]](
                id = s"lineUnits_${cell.row.id}",
                value = cell.value,
                compact = true,
                disabled = disabled,
                clazz = ExploreStyles.BrightnessesTableUnitsDropdown
              ),
            size = 120,
            minSize = 85,
            maxSize = 140
          ),
          ColDef(
            "delete",
            _._1,
            "",
            cell =>
              <.div(
                ExploreStyles.BrightnessesTableDeletButtonWrapper,
                Button(
                  size = Small,
                  clazz = ExploreStyles.DeleteButton,
                  disabled = disabled,
                  onClick = emissionLines.mod(_ - cell.value)
                )(Icons.Trash)
              ),
            size = 20,
            minSize = 20,
            maxSize = 20,
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
        getRowId = (row, _, _) => row._1.toPicometers.value.toString,
        enableSorting = true,
        enableColumnResizing = false,
        initialState = raw.mod
          .InitialTableState()
          .setSorting(List(raw.mod.ColumnSort(false, "wavelength")).toJSArray) // TODO Better facade
      )
    )
    // newWavelength
    .useStateView(none[Wavelength])
    // addDisabled
    .useStateView(AddDisabled(true))
    .render { (props, _, _, table, newWavelength, addDisabled) =>
      val bd1 = refineV[Positive](BigDecimal(1)).getOrElse(sys.error("Cannot happen"))

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
          "New line λ: ",
          FormInputEV[View, Option[Wavelength]](
            id = "newWavelength".refined,
            value = newWavelength,
            validFormat = InputValidSplitEpi.fromFormat(formatWavelengthMicron).optional,
            changeAuditor = ChangeAuditor
              .fromFormat(formatWavelengthMicron)
              .decimal(3.refined)
              .allow(List("0", "0.").contains)
              .optional,
            onTextChange = s => addDisabled.set(AddDisabled(s.isEmpty)),
            clazz = ExploreStyles.NewEmissionLineWavelength
          ),
          "μm",
          Button(
            size = Mini,
            compact = true,
            onClick = addLine,
            clazz = ExploreStyles.BrightnessAddButton,
            disabled = props.disabled || addDisabled.get.value
          )(
            Icons.New
          )
        )

      <.div(ExploreStyles.ExploreTable)(
        <.label("Brightness"),
        PrimeAutoHeightVirtualizedTable(
          table,
          estimateRowHeightPx = _ => 34,
          striped = true,
          compact = Compact.Very,
          emptyMessage = "No lines defined"
        ),
        footer
      )
    }

case class IntegratedEmissionLineEditor(
  emissionLines: View[SortedMap[Wavelength, EmissionLine[Integrated]]],
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
  disabled:      Boolean
) extends ReactFnProps[SurfaceEmissionLineEditor](SurfaceEmissionLineEditor.component)
    with EmissionLineEditor[Surface]

object SurfaceEmissionLineEditor
    extends EmissionLineEditorBuilder[Surface, SurfaceEmissionLineEditor] {

  val defaultLineUnits =
    summon[TaggedUnit[ErgsPerSecondCentimeter2Arcsec2, LineFlux[Surface]]].unit
}
