// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.Order._
import cats.syntax.all._
import crystal.react._
import crystal.react.hooks._
import crystal.react.implicits._
import crystal.react.reuse._
import eu.timepit.refined.auto._
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.model.ExploreModelValidators
import explore.model.display._
import japgolly.scalajs.react._
import japgolly.scalajs.react.callback.CallbackCats._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.enums.Band
import lucuma.core.math.BrightnessUnits._
import lucuma.core.math.dimensional._
import lucuma.core.util.Enumerated
import lucuma.ui.forms.EnumViewSelect
import lucuma.ui.forms.FormInputEV
import lucuma.ui.input.ChangeAuditor
import lucuma.ui.reusability._
import monocle.Focus
import react.common._
import react.common.implicits._
import react.semanticui.collections.table._
import react.semanticui.elements.button.Button
import react.semanticui.sizes._
import reactST.reactTable._
import reactST.reactTable.mod.SortingRule
import lucuma.refined.*

import scala.collection.immutable.SortedMap

sealed trait BrightnessesEditor[T] {
  val brightnesses: View[SortedMap[Band, BrightnessMeasure[T]]]
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

  private type RowValue = (Band, View[BrightnessMeasure[T]])

  private val BrightnessTableDef = TableDef[RowValue].withSortBy.withFlexLayout

  private val BrightnessTable = new SUITableVirtuoso(BrightnessTableDef)

  private val tableState = BrightnessTableDef.State().setSortBy(SortingRule("band"))

  val component =
    ScalaFnComponent
      .withHooks[Props]
      .useStateViewBy(props => State.fromUsedBrightnesses(props.brightnesses.get))
      .useEffectWithDepsBy((props, _) => props.brightnesses.get)((_, state) =>
        brightnesses => state.set(State.fromUsedBrightnesses(brightnesses))
      )
      .useMemoBy((props, _) => (props.brightnesses.reuseByValue, props.disabled)) {
        (_, _) => // Memo cols
          { case (brightnesses, disabled) =>
            List(
              BrightnessTableDef
                .Column("band", _._1)
                .setHeader("Band")
                .setCell(_.value.shortName)
                .setWidth(60)
                .setMinWidth(50)
                .setMaxWidth(60)
                .setSortByAuto,
              BrightnessTableDef
                .Column("value", _._2.zoom(Measure.valueTagged[BigDecimal, Brightness[T]]))
                .setHeader("Value")
                .setCell(cell =>
                  FormInputEV[View, BigDecimal](
                    id = NonEmptyString.unsafeFrom(s"brightnessValue_${cell.row.id}"),
                    value = cell.value,
                    validFormat = ExploreModelValidators.brightnessValidWedge,
                    changeAuditor =
                      ChangeAuditor.bigDecimal(2.refined, 3.refined).allowExp(2.refined),
                    disabled = disabled
                  )
                )
                .setWidth(80)
                .setMinWidth(60)
                .setMaxWidth(160),
              BrightnessTableDef
                .Column("units", _._2.zoom(Measure.unitsTagged[BigDecimal, Brightness[T]]))
                .setHeader("Units")
                .setCell(cell =>
                  EnumViewSelect[View, Units Of Brightness[T]](
                    id = NonEmptyString.unsafeFrom(s"brightnessUnits_${cell.row.id}"),
                    value = cell.value,
                    compact = true,
                    disabled = disabled,
                    clazz = ExploreStyles.BrightnessesTableUnitsDropdown
                  )
                )
                .setWidth(100)
                .setMinWidth(100)
                .setMaxWidth(160),
              BrightnessTableDef
                .Column("delete", _._1)
                .setCell(cell =>
                  <.div(ExploreStyles.BrightnessesTableDeletButtonWrapper)(
                    Button(
                      size = Small,
                      clazz = ExploreStyles.DeleteButton,
                      disabled = disabled,
                      onClick = brightnesses.mod(_ - cell.value)
                    )(Icons.Trash)
                  )
                )
                .setWidth(20)
                .setMinWidth(20)
                .setMaxWidth(20)
                .setDisableSortBy(true)
            )
          }
      }
      // rows
      .useMemoBy((props, _, _) => props.brightnesses.get)((props, _, _) =>
        _ => props.brightnesses.widen[Map[Band, BrightnessMeasure[T]]].toListOfViews
      )
      .useTableBy((_, _, cols, rows) =>
        BrightnessTableDef(
          cols,
          rows,
          ((_: BrightnessTableDef.OptionsType)
            .setRowIdFn(_._1.tag)
            .setInitialState(tableState))
            .reuseAlways
        )
      )
      .render { (props, state, _, _, tableInstance) =>
        val footer =
          <.div(
            ExploreStyles.BrightnessesTableFooter,
            state
              .zoom(State.newBand)
              .mapValue { (bandView: View[Band]) =>
                val addBrightness =
                  props.brightnesses.mod(brightnesses =>
                    brightnesses +
                      (bandView.get ->
                        defaultBandUnits(bandView.get).withValueTagged(BigDecimal(0)))
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
                  Button(
                    size = Mini,
                    compact = true,
                    onClick = addBrightness,
                    clazz = ExploreStyles.BrightnessAddButton,
                    disabled = props.disabled
                  )(
                    Icons.New
                  )
                )
              }
              .whenDefined
          )

        <.div(ExploreStyles.ExploreTable |+| ExploreStyles.BrightnessesTableContainer)(
          <.label(label),
          BrightnessTable.Component(
            table = Table(
              celled = true,
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
  brightnesses: View[SortedMap[Band, BrightnessMeasure[Integrated]]],
  disabled:     Boolean
) extends ReactFnProps[IntegratedBrightnessEditor](IntegratedBrightnessEditor.component)
    with BrightnessesEditor[Integrated]

object IntegratedBrightnessEditor
    extends BrightnessesEditorBuilder[Integrated, IntegratedBrightnessEditor] {
  protected val label                                                          = "Brightness"
  protected lazy val defaultBandUnits: Band => Units Of Brightness[Integrated] =
    _.defaultIntegrated.units
}

final case class SurfaceBrightnessEditor(
  brightnesses: View[SortedMap[Band, BrightnessMeasure[Surface]]],
  disabled:     Boolean
) extends ReactFnProps[SurfaceBrightnessEditor](SurfaceBrightnessEditor.component)
    with BrightnessesEditor[Surface]

object SurfaceBrightnessEditor extends BrightnessesEditorBuilder[Surface, SurfaceBrightnessEditor] {
  protected val label                                                       = "Surface Brightness"
  protected lazy val defaultBandUnits: Band => Units Of Brightness[Surface] =
    _.defaultSurface.units
}
