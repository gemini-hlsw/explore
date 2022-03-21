// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.Order._
import cats.syntax.all._
import crystal.ViewF
import crystal.react.View
import crystal.react.implicits._
import crystal.react.reuse._
import eu.timepit.refined.auto._
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.model.display._
import explore.utils.ReactTableHelpers
import japgolly.scalajs.react._
import japgolly.scalajs.react.callback.CallbackCats._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.enum.Band
import lucuma.core.math.BrightnessUnits._
import lucuma.core.math.BrightnessValue
import lucuma.core.math.dimensional._
import lucuma.core.util.Enumerated
import lucuma.ui.forms.EnumViewSelect
import lucuma.ui.optics.ChangeAuditor
import lucuma.ui.optics.ValidFormatInput
import lucuma.ui.reusability._
import monocle.Focus
import react.common._
import react.common.implicits._
import react.semanticui.collections.table._
import react.semanticui.elements.button.Button
import react.semanticui.sizes._
import reactST.reactTable._
import reactST.reactTable.mod.SortingRule
import explore.model.reusability._

import scala.collection.immutable.SortedMap

sealed trait BrightnessesEditor[T] {
  val brightnesses: Reuse[View[SortedMap[Band, BrightnessMeasure[T]]]]
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

  private type RowValue = (Band, View[BrightnessMeasure[T]])

  private val BrightnessTable = TableDef[RowValue].withSortBy

  private val BrightnessTableComponent = new SUITable(BrightnessTable)

  private val deleteButton = Button(
    size = Small,
    compact = true,
    clazz = ExploreStyles.DeleteButton
  )(
    Icons.Trash
  )

  private val tableState = BrightnessTable.State().setSortBy(SortingRule("band"))

  val component =
    ScalaFnComponent
      .withHooks[Props]
      .useStateBy(props => State.fromUsedBrightnesses(props.brightnesses.get)) // withReuse? !?!
      .useEffectWithDepsBy((props, _) => props.brightnesses.get)((_, state) =>
        brightnesses => state.setState(State.fromUsedBrightnesses(brightnesses))
      )
      .useMemoBy((props, _) => (props.brightnesses.get, props.disabled)) {
        (props, _) => // Memo cols
          _ =>
            val deleteFn: Band => Callback =
              b => props.brightnesses.mod(_ - b)

            List(
              BrightnessTable
                .Column("band", _._1)
                .setHeader("Band")
                .setCell(_.value.shortName)
                .setWidth(66)
                .setMinWidth(66)
                .setMaxWidth(66)
                .setSortByAuto,
              BrightnessTable
                .Column("value", _._2.zoom(Measure.valueTagged[BrightnessValue, Brightness[T]]))
                .setHeader("Value")
                .setCell(
                  ReactTableHelpers
                    .editableViewColumn(
                      validFormat = ValidFormatInput.fromFormat(
                        BrightnessValue.fromString,
                        "Invalid brightness value"
                      ),
                      changeAuditor = ChangeAuditor
                        .fromFormat(BrightnessValue.fromString)
                        .decimal(3)
                        .allowEmpty,
                      disabled = props.disabled
                    )
                ),
              BrightnessTable
                .Column("units", _._2.zoom(Measure.unitsTagged[BrightnessValue, Brightness[T]]))
                .setHeader("Units")
                .setCell(
                  ReactTableHelpers.editableEnumViewColumn[Units Of Brightness[T]](
                    disabled = props.disabled,
                    modifiers = List(ExploreStyles.BrightnessesTableUnitsDropdown)
                  )
                ),
              BrightnessTable
                .Column("delete", _._1)
                .setCell(
                  ReactTableHelpers.buttonViewColumn(
                    button = deleteButton,
                    onClick = deleteFn,
                    disabled = props.disabled,
                    wrapperClass = ExploreStyles.BrightnessesTableDeletButtonWrapper
                  )
                )
                .setWidth(46)
                .setMinWidth(46)
                .setMaxWidth(46)
                .setDisableSortBy(true)
            )
      }
      // rows
      .useMemoBy((props, _, _) => props.brightnesses)((props, _, _) =>
        _ => props.brightnesses.value.widen[Map[Band, BrightnessMeasure[T]]].toListOfViews
      )
      .useTableBy((_, _, cols, rows) =>
        BrightnessTable(cols,
                        rows,
                        ((_: BrightnessTable.OptionsType)
                          .setRowIdFn(_._1.tag)
                          .setInitialState(tableState))
                          .reuseAlways
        )
      )
      .renderWithReuse { (props, state, _, _, tableInstance) =>
        println("RENDERING BRIGHTNESS TABLE")

        val newBandView: Option[View[Band]] =
          state.value.newBand.map(band =>
            ViewF(
              band,
              (mod, _) =>
                // This View will ignore Callbacks. This is OK as long as noone calls its .withOnMod.
                state.modState(State.newBand.some.modify(mod))
            )
          )

        val footer =
          <.div(
            ExploreStyles.BrightnessesTableFooter,
            newBandView.whenDefined { bandView =>
              val addBrightness =
                props.brightnesses.mod(brightnesses =>
                  (brightnesses +
                    (bandView.get ->
                      defaultBandUnits(bandView.get).withValueTagged(BrightnessValue(0))))
                )

              React.Fragment(
                EnumViewSelect(
                  id = "NEW_BAND",
                  value = bandView,
                  exclude = state.value.usedBands,
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
          )

        <.div(ExploreStyles.ExploreTable |+| ExploreStyles.BrightnessesTableContainer)(
          <.label(label),
          BrightnessTableComponent(
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
  brightnesses: Reuse[View[SortedMap[Band, BrightnessMeasure[Integrated]]]],
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
  brightnesses: Reuse[View[SortedMap[Band, BrightnessMeasure[Surface]]]],
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
