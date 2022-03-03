// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.Order._
import cats.syntax.all._
import coulomb._
import crystal.react.View
import crystal.react.hooks._
import crystal.react.implicits._
import crystal.react.reuse._
import eu.timepit.refined.auto._
import eu.timepit.refined.cats._
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
import lucuma.ui.forms.EnumViewSelect
import lucuma.ui.forms.FormInputEV
import lucuma.ui.optics.AuditResult
import lucuma.ui.optics.ChangeAuditor
import lucuma.ui.optics.ValidFormatInput
import lucuma.ui.reusability._
import react.common._
import react.common.implicits._
import react.semanticui.collections.table._
import react.semanticui.elements.button.Button
import react.semanticui.sizes._
import reactST.reactTable._
import reactST.reactTable.mod.SortingRule

import scala.collection.immutable.SortedMap

sealed trait EmissionLineEditor[T] {
  val emissionLines: View[SortedMap[Wavelength, EmissionLine[T]]]
  val disabled: Boolean
}

sealed abstract class EmissionLineEditorBuilder[T, Props <: EmissionLineEditor[T]](implicit
  enumUnits: Enumerated[Units Of LineFlux[T]]
) {
  implicit protected def propsReuse: Reusability[Props] // Abstract

  private val defaultLineUnits: Units Of LineFlux[T] = enumUnits.all.head

  private type RowValue = (Wavelength, View[EmissionLine[T]])

  private val EmissionLineTable = TableDef[RowValue].withSortBy

  private val EmissionLineTableComponent = new SUITable(EmissionLineTable)

  private val tableState = EmissionLineTable.State().setSortBy(SortingRule("wavelength"))

  val component = ScalaFnComponent
    .withHooks[Props]
    .useMemoBy(props => (props.emissionLines, props.disabled)) { _ => // Memo cols
      { case (emissionLines, disabled) =>
        List(
          EmissionLineTable
            .Column("wavelength", _._1)
            .setHeader(_ => <.span(ExploreStyles.TextPlain, "λ (µm)"))
            .setCell(cell =>
              Wavelength.decimalMicrometers.reverseGet(cell.value).setScale(3).toString
            )
            .setWidth(80)
            .setMinWidth(80)
            .setMaxWidth(80)
            .setSortByAuto,
          EmissionLineTable
            .Column(
              "width",
              _._2.zoom(EmissionLine.lineWidth[T]).stripQuantity
            )
            .setHeader("Width (km/s)")
            .setCell(cell =>
              FormInputEV[View, PosBigDecimal](
                id = NonEmptyString.unsafeFrom(s"lineWidth_${cell.row.id}"),
                value = cell.value,
                validFormat = ValidFormatInput.fromFormat(formatPosBigDecimal),
                changeAuditor = ChangeAuditor.fromFormat(formatPosBigDecimal).decimal(3).allowEmpty,
                disabled = disabled
              )
            ),
          EmissionLineTable
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
                validFormat = ValidFormatInput.fromFormat(formatPosBigDecimal),
                changeAuditor = ChangeAuditor.fromFormat(formatPosBigDecimal).decimal(3).allowEmpty,
                disabled = disabled
              )
            ),
          EmissionLineTable
            .Column(
              "lineUnits",
              _._2.zoom(
                EmissionLine.lineFlux.andThen(Measure.unitsTagged[PosBigDecimal, LineFlux[T]])
              )
            )
            .setHeader("Units")
            .setCell(cell =>
              EnumViewSelect[View, Units Of LineFlux[T]](
                id = NonEmptyString.unsafeFrom(s"lineUnits_${cell.row.id}"),
                value = cell.value,
                compact = true,
                disabled = disabled,
                clazz = ExploreStyles.BrightnessesTableUnitsDropdown
              )
            )
            .setMaxWidth(60),
          EmissionLineTable
            .Column("delete", _._1)
            .setCell(cell =>
              <.div(
                ExploreStyles.BrightnessesTableDeletButtonWrapper,
                Button(
                  size = Small,
                  compact = true,
                  clazz = ExploreStyles.DeleteButton,
                  disabled = disabled,
                  onClick = emissionLines.mod(_ - cell.value)
                )(Icons.Trash)
              )
            )
            .setWidth(46)
            .setMinWidth(46)
            .setMaxWidth(46)
            .setDisableSortBy(true)
        )
      }
    }
    // rows
    .useMemoBy((props, _) => props.emissionLines)((_, _) =>
      _.widen[Map[Wavelength, EmissionLine[T]]].toListOfViews
    )
    .useTableBy((_, cols, rows) =>
      EmissionLineTable(cols,
                        rows,
                        ((_: EmissionLineTable.OptionsType)
                          .setRowIdFn(_._1.toPicometers.value.toString)
                          .setInitialState(tableState))
                          .reuseAlways
      )
    )
    // newWavelength
    .useStateView(none[Wavelength])
    // addDisabled
    .useStateView(true)
    .renderWithReuse { (props, _, _, tableInstance, newWavelength, addDisabled) =>
      val addLine =
        newWavelength.get.foldMap(wavelength =>
          props.emissionLines.mod(emissionLines =>
            (emissionLines +
              (wavelength -> EmissionLine(
                PosBigDecimal(BigDecimal(1)).withUnit[KilometersPerSecond],
                defaultLineUnits.withValueTagged(BigDecimal(1))
              )))
          ) >> newWavelength.set(none)
        )

      // TODO Move to lucuma-ui
      /**
       * Unconditionally allows the field to be a zero. This is useful when using a ChangeAuditor
       * made from a Format that only accepts positive numbers, but you want the user to be able to
       * enter decimal numbers.
       */
      def allowZero[A](self: ChangeAuditor[A]): ChangeAuditor[A] =
        ChangeAuditor { (s, c) =>
          if (s == "0" || s == "0.") AuditResult.accept else self.audit(s, c)
        }

      val footer =
        <.div(
          ExploreStyles.BrightnessesTableFooter,
          "New line λ: ",
          FormInputEV(
            id = "newWavelength",
            value = newWavelength,
            validFormat = ValidFormatInput.fromFormatOptional(formatWavelengthMicron),
            changeAuditor =
              allowZero(ChangeAuditor.fromFormat(formatWavelengthMicron).decimal(3)).optional,
            onTextChange = s => addDisabled.set(s.isEmpty),
            clazz = ExploreStyles.NewEmissionLineWavelength
          ),
          "μm",
          Button(size = Mini,
                 compact = true,
                 onClick = addLine,
                 disabled = props.disabled || addDisabled.get
          )(
            ^.marginLeft := "5px"
          )(
            Icons.New
          )
        )

      <.div(ExploreStyles.ExploreTable |+| ExploreStyles.BrightnessesTableContainer)(
        <.label("Brightness"),
        EmissionLineTableComponent(
          table = Table(celled = true,
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
    extends EmissionLineEditorBuilder[Integrated, IntegratedEmissionLineEditor] {
  implicit protected lazy val propsReuse: Reusability[IntegratedEmissionLineEditor] =
    Reusability.derive
}

final case class SurfaceEmissionLineEditor(
  emissionLines: View[SortedMap[Wavelength, EmissionLine[Surface]]],
  disabled:      Boolean
) extends ReactFnProps[SurfaceEmissionLineEditor](SurfaceEmissionLineEditor.component)
    with EmissionLineEditor[Surface]

object SurfaceEmissionLineEditor
    extends EmissionLineEditorBuilder[Surface, SurfaceEmissionLineEditor] {
  implicit protected lazy val propsReuse: Reusability[SurfaceEmissionLineEditor] =
    Reusability.derive
}
