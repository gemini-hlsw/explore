// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.Order._
import cats.syntax.all._
import coulomb._
import coulomb.syntax.*
import crystal.react.View
import crystal.react.hooks._
import crystal.react.implicits._
import crystal.react.reuse._
import eu.timepit.refined._
import eu.timepit.refined.cats._
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.types.numeric.PosBigDecimal
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.model.formats._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.math.BrightnessUnits._
import lucuma.core.math.Wavelength
import lucuma.core.math.dimensional._
import lucuma.core.math.units._
import lucuma.core.model.EmissionLine
import lucuma.core.util.Enumerated
import lucuma.core.validation._
import lucuma.refined.*
import lucuma.ui.forms.EnumViewSelect
import lucuma.ui.forms.FormInputEV
import lucuma.ui.input.ChangeAuditor
import lucuma.ui.reusability._
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import react.common.ReactFnProps
import react.semanticui.collections.table._
import react.semanticui.elements.button.Button
import react.semanticui.sizes._
import reactST.reactTable._
import reactST.reactTable.mod.SortingRule

import scala.collection.immutable.SortedMap
import scala.math.BigDecimal.RoundingMode

sealed trait EmissionLineEditor[T] {
  val emissionLines: View[SortedMap[Wavelength, EmissionLine[T]]]
  val disabled: Boolean
}

sealed abstract class EmissionLineEditorBuilder[T, Props <: EmissionLineEditor[T]](implicit
  enumUnits: Enumerated[Units Of LineFlux[T]]
) {
  private val defaultLineUnits: Units Of LineFlux[T] = enumUnits.all.head

  private type RowValue = (Wavelength, View[EmissionLine[T]])

  private val EmissionLineTableRef = TableDef[RowValue].withSortBy.withFlexLayout

  private val EmissionLineTable = new SUITableVirtuoso(EmissionLineTableRef)

  private val tableState = EmissionLineTableRef.State().setSortBy(SortingRule("wavelength"))

  val component = ScalaFnComponent
    .withHooks[Props]
    .useMemoBy(props => (props.emissionLines.reuseByValue, props.disabled)) { _ => // Memo cols
      { case (emissionLines, disabled) =>
        List(
          EmissionLineTableRef
            .Column("wavelength", _._1)
            .setHeader(_ => <.span(ExploreStyles.TextPlain, "λ (µm)"))
            .setCell(cell =>
              Wavelength.decimalMicrometers
                .reverseGet(cell.value)
                .setScale(3, RoundingMode.HALF_UP)
                .toString
            )
            .setWidth(60)
            .setMinWidth(50)
            .setMaxWidth(80)
            .setSortByAuto,
          EmissionLineTableRef
            .Column(
              "width",
              _._2.zoom(EmissionLine.lineWidth[T]).stripQuantity
            )
            .setHeader("Width (km/s)")
            .setCell(cell =>
              FormInputEV[View, PosBigDecimal](
                id = NonEmptyString.unsafeFrom(s"lineWidth_${cell.row.id}"),
                value = cell.value,
                validFormat = InputValidSplitEpi.posBigDecimal,
                changeAuditor = ChangeAuditor.posBigDecimal(3.refined).allowEmpty,
                disabled = disabled
              )
            )
            .setWidth(90)
            .setMinWidth(80)
            .setMaxWidth(160),
          EmissionLineTableRef
            .Column(
              "lineValue",
              _._2.zoom(
                EmissionLine.lineFlux.andThen(Measure.valueTagged[PosBigDecimal, LineFlux[T]])
              )
            )
            .setHeader("Brightness")
            .setCell(cell =>
              FormInputEV[View, PosBigDecimal](
                id = NonEmptyString.unsafeFrom(s"lineValue_${cell.row.id}"),
                value = cell.value,
                validFormat = InputValidSplitEpi.posBigDecimalWithScientificNotation,
                changeAuditor = ChangeAuditor.posScientificNotation(),
                disabled = disabled
              )
            )
            .setWidth(80)
            .setMinWidth(70)
            .setMaxWidth(160),
          EmissionLineTableRef
            .Column(
              "lineUnits",
              _._2.zoom(
                EmissionLine.lineFlux.andThen(Measure.unitsTagged[PosBigDecimal, LineFlux[T]])
              )
            )
            .setHeader("Units")
            .setCell(cell =>
              EnumViewSelect[View, Units Of LineFlux[T]](
                id = s"lineUnits_${cell.row.id}",
                value = cell.value,
                compact = true,
                disabled = disabled,
                clazz = ExploreStyles.BrightnessesTableUnitsDropdown
              )
            )
            .setWidth(80)
            .setMinWidth(40)
            .setMaxWidth(80),
          EmissionLineTableRef
            .Column("delete", _._1)
            .setCell(cell =>
              <.div(
                ExploreStyles.BrightnessesTableDeletButtonWrapper,
                Button(
                  size = Small,
                  clazz = ExploreStyles.DeleteButton,
                  disabled = disabled,
                  onClick = emissionLines.mod(_ - cell.value)
                )(Icons.Trash)
              )
            )
            .setWidth(25)
            .setMinWidth(25)
            .setMaxWidth(25)
            .setDisableSortBy(true)
        )
      }
    }
    // rows
    .useMemoBy((props, _) => props.emissionLines.get)((props, _) =>
      _ => props.emissionLines.widen[Map[Wavelength, EmissionLine[T]]].toListOfViews
    )
    .useTableBy((_, cols, rows) =>
      EmissionLineTableRef(
        cols,
        rows,
        ((_: EmissionLineTableRef.OptionsType)
          .setRowIdFn(_._1.toPicometers.value.toString)
          .setInitialState(tableState))
          .reuseAlways
      )
    )
    // newWavelength
    .useStateView(none[Wavelength])
    // addDisabled
    .useStateView(true)
    .render { (props, _, _, tableInstance, newWavelength, addDisabled) =>
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
            onTextChange = s => addDisabled.set(s.isEmpty),
            clazz = ExploreStyles.NewEmissionLineWavelength
          ),
          "μm",
          Button(
            size = Mini,
            compact = true,
            onClick = addLine,
            clazz = ExploreStyles.BrightnessAddButton,
            disabled = props.disabled || addDisabled.get
          )(
            Icons.New
          )
        )

      <.div(ExploreStyles.ExploreTable |+| ExploreStyles.BrightnessesTableContainer)(
        <.label("Brightness"),
        EmissionLineTable.Component(
          table = Table(
            celled = true,
            selectable = true,
            striped = true,
            unstackable = true,
            compact = TableCompact.Very
          ),
          header = TableHeader(),
          emptyMessage = "No lines defined"
        )(tableInstance),
        footer
      )
    }

}

final case class IntegratedEmissionLineEditor(
  emissionLines: View[SortedMap[Wavelength, EmissionLine[Integrated]]],
  disabled:      Boolean
) extends ReactFnProps[IntegratedEmissionLineEditor](IntegratedEmissionLineEditor.component)
    with EmissionLineEditor[Integrated]

object IntegratedEmissionLineEditor
    extends EmissionLineEditorBuilder[Integrated, IntegratedEmissionLineEditor]

final case class SurfaceEmissionLineEditor(
  emissionLines: View[SortedMap[Wavelength, EmissionLine[Surface]]],
  disabled:      Boolean
) extends ReactFnProps[SurfaceEmissionLineEditor](SurfaceEmissionLineEditor.component)
    with EmissionLineEditor[Surface]

object SurfaceEmissionLineEditor
    extends EmissionLineEditorBuilder[Surface, SurfaceEmissionLineEditor]
