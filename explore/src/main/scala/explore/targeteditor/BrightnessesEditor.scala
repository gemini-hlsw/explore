// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.Order.*
import cats.syntax.all.*
import crystal.react.*
import crystal.react.hooks.*
import crystal.react.implicits.*
import crystal.react.reuse.*
import eu.timepit.refined.auto.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.*
import explore.components.ui.ExploreStyles
import explore.given
import explore.model.ExploreModelValidators
import explore.model.display.given
import japgolly.scalajs.react.*
import japgolly.scalajs.react.callback.CallbackCats.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.Band
import lucuma.core.math.BrightnessUnits.*
import lucuma.core.math.dimensional.*
import lucuma.core.util.Enumerated
import lucuma.react.table.*
import lucuma.refined.*
import lucuma.ui.forms.EnumViewSelect
import lucuma.ui.forms.FormInputEV
import lucuma.ui.input.ChangeAuditor
import lucuma.ui.reusability.*
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import lucuma.ui.table.*
import monocle.Focus
import react.common.ReactFnProps
import react.semanticui.collections.table.*
import react.semanticui.elements.button.Button
import react.semanticui.sizes.*
import reactST.{tanstackTableCore => raw}

import scala.collection.immutable.SortedMap

import scalajs.js.JSConverters.*

sealed trait BrightnessesEditor[T] {
  val brightnesses: View[SortedMap[Band, BrightnessMeasure[T]]]
  val disabled: Boolean
}

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

  private val ColDef = ColumnDef[RowValue]

  val component =
    ScalaFnComponent
      .withHooks[Props]
      .useStateViewBy(props => State.fromUsedBrightnesses(props.brightnesses.get))
      .useEffectWithDepsBy((props, _) => props.brightnesses.get)((_, state) =>
        brightnesses => state.set(State.fromUsedBrightnesses(brightnesses))
      )
      .useMemoBy((props, _) => (props.brightnesses.reuseByValue, props.disabled)) {
        (_, _) => // Memo cols
          case (brightnesses, disabled) =>
            List(
              ColDef(
                "band",
                _._1,
                "Band",
                _.value.shortName,
                size = 60,
                minSize = 50,
                maxSize = 60
              ).sortable,
              ColDef(
                "value",
                _._2.zoom(Measure.valueTagged[BigDecimal, Brightness[T]]),
                "Value",
                cell =>
                  FormInputEV[View, BigDecimal](
                    id = NonEmptyString.unsafeFrom(s"brightnessValue_${cell.row.id}"),
                    value = cell.value,
                    validFormat = ExploreModelValidators.brightnessValidWedge,
                    changeAuditor =
                      ChangeAuditor.bigDecimal(2.refined, 3.refined).allowExp(2.refined),
                    disabled = disabled
                  ),
                size = 80,
                minSize = 60,
                maxSize = 160
              ).sortableBy(_.get),
              ColDef(
                "units",
                _._2.zoom(Measure.unitsTagged[BigDecimal, Brightness[T]]),
                "Units",
                cell =>
                  EnumViewSelect[View, Units Of Brightness[T]](
                    id = NonEmptyString.unsafeFrom(s"brightnessUnits_${cell.row.id}"),
                    value = cell.value,
                    compact = true,
                    disabled = disabled,
                    clazz = ExploreStyles.BrightnessesTableUnitsDropdown
                  ),
                size = 100,
                minSize = 100,
                maxSize = 160
              ).sortableBy(_.get),
              ColDef(
                "delete",
                _._1,
                "",
                cell =>
                  <.div(ExploreStyles.BrightnessesTableDeletButtonWrapper)(
                    Button(
                      size = Small,
                      clazz = ExploreStyles.DeleteButton,
                      disabled = disabled,
                      onClick = brightnesses.mod(_ - cell.value)
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
      .useMemoBy((props, _, _) => props.brightnesses.get)((props, _, _) =>
        _ => props.brightnesses.widen[Map[Band, BrightnessMeasure[T]]].toListOfViews
      )
      .useReactTableBy((_, _, cols, rows) =>
        TableOptions(
          cols,
          rows,
          getRowId = (row, _, _) => row._1.tag,
          enableSorting = true,
          enableColumnResizing = false,
          initialState = raw.mod
            .InitialTableState()
            .setSorting(List(raw.mod.ColumnSort(false, "band")).toJSArray) // TODO Better facade
        )
      )
      .render { (props, state, _, _, table) =>
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

        <.div(ExploreStyles.ExploreTable |+| ExploreStyles.BrightnessesContainer)(
          <.label(label),
          PrimeAutoHeightVirtualizedTable(
            table,
            estimateRowHeightPx = _ => 34,
            striped = true,
            compact = Compact.Very,
            emptyMessage = "No brightnesses defined"
          ),
          footer
        )

      }

case class IntegratedBrightnessEditor(
  brightnesses: View[SortedMap[Band, BrightnessMeasure[Integrated]]],
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
  disabled:     Boolean
) extends ReactFnProps[SurfaceBrightnessEditor](SurfaceBrightnessEditor.component)
    with BrightnessesEditor[Surface]

object SurfaceBrightnessEditor extends BrightnessesEditorBuilder[Surface, SurfaceBrightnessEditor]:
  protected val label                                                       = "Surface Brightness"
  protected lazy val defaultBandUnits: Band => Units Of Brightness[Surface] =
    _.defaultSurface.units
