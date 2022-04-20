// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.Order._
import cats.syntax.all._
import crystal.react._
import crystal.react.hooks._
import crystal.react.reuse._
import eu.timepit.refined.auto._
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.model.display._
import japgolly.scalajs.react._
import japgolly.scalajs.react.callback.CallbackCats._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.enum.Band
import lucuma.core.math.BrightnessUnits._
import lucuma.core.math.dimensional._
import lucuma.core.util.Enumerated
import lucuma.ui.forms.EnumViewSelect
import lucuma.ui.forms.FormInputEV
import lucuma.ui.optics.ChangeAuditor
import lucuma.ui.optics.ValidFormatInput
import lucuma.ui.reusability._
import monocle.Focus
import react.common._
import react.common.implicits._
import react.semanticui.collections.table._
import react.semanticui.elements.button.Button
import react.semanticui.sizes._
import react.virtuoso._
import reactST.reactTable._
import reactST.reactTable.mod.SortingRule

import scala.collection.immutable.SortedMap
import scala.util.Try

sealed trait BrightnessesEditor[T] {
  val brightnesses: ReuseView[SortedMap[Band, BrightnessMeasure[T]]]
  val disabled: Boolean
}

sealed abstract class BrightnessesEditorBuilder[T, Props <: BrightnessesEditor[T]](implicit
  enumUnits: Enumerated[Units Of Brightness[T]]
) {
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

  implicit protected def propsReuse: Reusability[Props] // Abstract
  implicit protected val stateReuse: Reusability[State] = Reusability.derive

  private type RowValue = (Band, ReuseView[BrightnessMeasure[T]])

  private val BrightnessTableDef =
    TableDef[RowValue].withSortBy.withFlexLayout // .withBlockLayout

  private val BrightnessTable = new SUITableVirtuoso(BrightnessTableDef)

  private val tableState = BrightnessTableDef.State().setSortBy(SortingRule("band"))

  private val validBrightnessValue: ValidFormatInput[BigDecimal] =
    ValidFormatInput(
      ValidFormatInput.bigDecimalValidFormat("Invalid brightness value").getValidated,
      n => Try(displayBrightness.shortName(n)).toOption.orEmpty
    )

  val component =
    ScalaFnComponent
      .withHooks[Props]
      .useStateViewWithReuseBy(props => State.fromUsedBrightnesses(props.brightnesses.get))
      .useEffectWithDepsBy((props, _) => props.brightnesses.get)((_, state) =>
        brightnesses => state.set(State.fromUsedBrightnesses(brightnesses))
      )
      .useMemoBy((props, _) => (props.brightnesses, props.disabled)) { (_, _) => // Memo cols
        { case (brightnesses, disabled) =>
          List(
            BrightnessTableDef
              .Column("band", _._1)
              .setHeader("Band")
              .setCell(_.value.shortName)
              .setWidth(66)
              .setMinWidth(66)
              .setMaxWidth(66)
              .setSortByAuto,
            BrightnessTableDef
              .Column("value", _._2.zoom(Measure.valueTagged[BigDecimal, Brightness[T]]))
              .setHeader("Value")
              .setCell(cell =>
                FormInputEV[ReuseView, BigDecimal](
                  id = NonEmptyString.unsafeFrom(s"brightnessValue_${cell.row.id}"),
                  value = cell.value,
                  validFormat = validBrightnessValue,
                  changeAuditor = ChangeAuditor.bigDecimal(2, 3).allowExp(2),
                  disabled = disabled
                )
              ),
            BrightnessTableDef
              .Column("units", _._2.zoom(Measure.unitsTagged[BigDecimal, Brightness[T]]))
              .setHeader("Units")
              .setCell(cell =>
                EnumViewSelect[ReuseView, Units Of Brightness[T]](
                  id = NonEmptyString.unsafeFrom(s"brightnessUnits_${cell.row.id}"),
                  value = cell.value,
                  compact = true,
                  disabled = disabled,
                  clazz = ExploreStyles.BrightnessesTableUnitsDropdown
                )
              ),
            BrightnessTableDef
              .Column("delete", _._1)
              .setCell(cell =>
                <.div(ExploreStyles.BrightnessesTableDeletButtonWrapper)(
                  Button(
                    size = Small,
                    compact = true,
                    clazz = ExploreStyles.DeleteButton,
                    disabled = disabled,
                    onClick = brightnesses.mod(_ - cell.value)
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
      .useMemoBy((props, _, _) => props.brightnesses)((_, _, _) =>
        _.widen[Map[Band, BrightnessMeasure[T]]].toListOfViews
      )
      .useTableBy((_, _, cols, rows) =>
        BrightnessTableDef(cols,
                           rows,
                           ((_: BrightnessTableDef.OptionsType)
                             .setRowIdFn(_._1.tag)
                             .setInitialState(tableState))
                             .reuseAlways
        )
      )
      .renderWithReuse { (props, state, _, _, tableInstance) =>
        val footer =
          <.div(
            ExploreStyles.BrightnessesTableFooter,
            state
              .zoom(State.newBand)
              .mapValue { (bandView: ReuseView[Band]) =>
                val addBrightness =
                  props.brightnesses.mod(brightnesses =>
                    (brightnesses +
                      (bandView.get ->
                        defaultBandUnits(bandView.get).withValueTagged(BigDecimal(0))))
                  )

                React.Fragment(
                  EnumViewSelect(
                    id = "NEW_BAND",
                    value = bandView,
                    exclude = state.get.usedBands,
                    upward = true,
                    clazz = ExploreStyles.FlatFormField,
                    disabled = props.disabled
                  ),
                  Button(size = Mini,
                         compact = true,
                         onClick = addBrightness,
                         disabled = props.disabled
                  )(^.marginLeft := "5px")(
                    Icons.New
                  )
                )
              }
              .whenDefined
          )

        <.div(ExploreStyles.ExploreTable |+| ExploreStyles.BrightnessesTableContainer)(
          <.label(label),
          BrightnessTable.Component(
            table = Table(celled = true,
                          selectable = true,
                          striped = true,
                          unstackable = true,
                          compact = TableCompact.Very
            ),
            header = TableHeader(),
            emptyMessage = "No brightnesses defined"
          )(tableInstance),
          footer
        )

      }
}

final case class IntegratedBrightnessEditor(
  brightnesses: ReuseView[SortedMap[Band, BrightnessMeasure[Integrated]]],
  disabled:     Boolean
) extends ReactFnProps[IntegratedBrightnessEditor](IntegratedBrightnessEditor.component)
    with BrightnessesEditor[Integrated]

object IntegratedBrightnessEditor
    extends BrightnessesEditorBuilder[Integrated, IntegratedBrightnessEditor] {
  protected val label                                                             = "Brightness"
  protected lazy val defaultBandUnits: Band => Units Of Brightness[Integrated]    =
    _.defaultIntegrated.units
  implicit protected lazy val propsReuse: Reusability[IntegratedBrightnessEditor] =
    Reusability.derive
}

final case class SurfaceBrightnessEditor(
  brightnesses: ReuseView[SortedMap[Band, BrightnessMeasure[Surface]]],
  disabled:     Boolean
) extends ReactFnProps[SurfaceBrightnessEditor](SurfaceBrightnessEditor.component)
    with BrightnessesEditor[Surface]

object SurfaceBrightnessEditor extends BrightnessesEditorBuilder[Surface, SurfaceBrightnessEditor] {
  protected val label                                                          = "Surface Brightness"
  protected lazy val defaultBandUnits: Band => Units Of Brightness[Surface]    =
    _.defaultSurface.units
  implicit protected lazy val propsReuse: Reusability[SurfaceBrightnessEditor] =
    Reusability.derive
}
