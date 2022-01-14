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
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.ui.forms.EnumViewSelect
import lucuma.ui.optics.ChangeAuditor
import lucuma.ui.optics.ValidFormatInput
import lucuma.ui.reusability._
import monocle.Focus
import react.common._
import react.semanticui.collections.form.Form
import react.semanticui.collections.table._
import react.semanticui.elements.button.Button
import react.semanticui.elements.segment.Segment
import react.semanticui.elements.segment.SegmentAttached
import react.semanticui.sizes._
import reactST.reactTable._
import reactST.reactTable.mod.SortingRule

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

  protected def defaultBandUnits: Band => Units Of Brightness[T] // Abstract

  implicit protected def propsReuse: Reusability[Props] // Abstract
  implicit protected val stateReuse: Reusability[State] = Reusability.derive

  implicit protected val displayTaggedUnits: Display[Units Of Brightness[T]] = Display[Units].narrow

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
      .useStateBy(props => State.fromUsedBrightnesses(props.brightnesses.get))
      .useEffectWithDepsBy((props, _) => props.brightnesses)((_, state) =>
        brightnesses => state.setState(State.fromUsedBrightnesses(brightnesses.get))
      )
      .useMemoBy((props, _) => (props.brightnesses, props.disabled)) { (_, _) => // Memo cols
        { case (brightnesses, disabled) =>
          val deleteFn: RowValue => Callback =
            row => brightnesses.mod(_ - row._1)

          List(
            BrightnessTable
              .Column("band", _._1)
              .setHeader("Band")
              .setCell(_.value.shortName)
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
                    disabled = disabled
                  )
              ),
            BrightnessTable
              .Column("units", _._2.zoom(Measure.unitsTagged[BrightnessValue, Brightness[T]]))
              .setHeader("Units")
              .setCell(
                ReactTableHelpers.editableEnumViewColumn[Units Of Brightness[T]](
                  disabled = disabled,
                  modifiers = List(ExploreStyles.BrightnessesTableUnitsDropdown)
                )
              ),
            BrightnessTable
              .Column("delete")
              .setCell(
                ReactTableHelpers.buttonViewColumn(
                  button = deleteButton,
                  onClick = deleteFn,
                  disabled = disabled,
                  wrapperClass = ExploreStyles.BrightnessesTableDeletButtonWrapper
                )
              )
          )
        }
      }
      // rows
      .useMemoBy((props, _, _) => props.brightnesses)((_, _, _) =>
        _.widen[Map[Band, BrightnessMeasure[T]]].toListOfViews
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
        val newBandView: Option[View[Band]] =
          state.value.newBand.map(band =>
            ViewF(
              band,
              (mod, _) =>
                // This View will ignore Callbacks. This is OK as long as noone calls its .withOnMod.
                // .withOnMod will likely become deprecated in the transition to hooks.
                state.modState(State.newBand.some.modify(mod))
            )
          )

        val footer = TableFooter(
          TableRow(
            TableHeaderCell()(^.colSpan := 4)(
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

                  React.Fragment(
                    EnumViewSelect(
                      id = "NEW_BAND",
                      value = bandView,
                      exclude = state.value.usedBands,
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
            )
          )
        )

        // Put it inside a form to get the SUI styles right
        Form(as = <.div, size = Small)(
          <.div(
            ExploreStyles.BrightnessesTableSection,
            <.label("Brightnesses"),
            Segment(attached = SegmentAttached.Attached,
                    compact = true,
                    clazz = ExploreStyles.BrightnessesTableContainer
            )(
              BrightnessTableComponent(
                Table(celled = true,
                      selectable = true,
                      striped = true,
                      compact = TableCompact.Very
                ),
                header = TableHeader(),
                footer = footer.vdomElement
              )(tableInstance)
            )
          )
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
  protected lazy val defaultBandUnits: Band => Units Of Brightness[Integrated]    =
    _.defaultIntegrated.units
  implicit protected lazy val propsReuse: Reusability[IntegratedBrightnessEditor] =
    Reusability.derive
}
