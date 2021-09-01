// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.Order._
import cats.effect.SyncIO
import cats.syntax.all._
import crystal.ViewF
import crystal.react.implicits._
import eu.timepit.refined.auto._
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.model.display._
import explore.utils.ReactTableHelpers
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.enum.MagnitudeBand
import lucuma.core.math.MagnitudeValue
import lucuma.core.model.Magnitude
import lucuma.core.model.Target
import lucuma.ui.forms.EnumViewSelect
import lucuma.ui.optics.ChangeAuditor
import lucuma.ui.optics.ValidFormatInput
import lucuma.ui.reusability._
import monocle.Focus
import react.semanticui.collections.form.Form
import react.semanticui.collections.table._
import react.semanticui.elements.button.Button
import react.semanticui.elements.segment.Segment
import react.semanticui.elements.segment.SegmentAttached
import react.semanticui.sizes._
import reactST.reactTable.SUITable
import reactST.reactTable.TableDef
import reactST.reactTable.TableHooks.Implicits._
import reactST.reactTable.mod.ColumnInterface
import reactST.reactTable.mod.SortingRule

import scala.collection.immutable.HashSet

final case class MagnitudeForm(
  targetId:   Target.Id,
  magnitudes: View[List[Magnitude]],
  disabled:   Boolean
) //extends ReactProps[MagnitudeForm](MagnitudeForm.component)

object MagnitudeForm {
  type Props = MagnitudeForm

  protected case class State(usedBands: Set[MagnitudeBand], newBand: Option[MagnitudeBand])

  object State {
    val usedBands = Focus[State](_.usedBands)
    val newBand   = Focus[State](_.newBand)

    def fromUsedMagnitudes(magnitudes: List[Magnitude]): State = {
      val usedBands = HashSet.from(magnitudes.map(_.band))
      State(usedBands, MagnitudeBand.all.diff(usedBands.toList).headOption)
    }
  }

  implicit val propsReuse: Reusability[Props] = Reusability.derive
  implicit val stateReuse: Reusability[State] = Reusability.derive

  implicit def render(props: Props): VdomElement = component(props).vdomElement

  private val MagTable = TableDef[View[Magnitude]].withSort

  import MagTable.syntax._

  implicit private val colReuse: Reusability[List[ColumnInterface[View[Magnitude]]]] =
    Reusability.always
  implicit private val dataReuse: Reusability[List[View[Magnitude]]]                 = Reusability.byRefOr_==

  private val MagTableComponent = new SUITable(MagTable)

  private val deleteButton = Button(
    size = Small,
    compact = true,
    clazz = ExploreStyles.DeleteButton
  )(
    Icons.Trash
  )

  private val tableState = MagTable.State().setSortByVarargs(SortingRule("band"))

  val component =
    ScalaFnComponent
      .withHooks[Props]
      .useStateBy(props => State.fromUsedMagnitudes(props.magnitudes.get))
      .useEffectWithDepsBy((props, _) => props.magnitudes)((_, state) =>
        magnitudes => state.setState(State.fromUsedMagnitudes(magnitudes.get))
      )
      .useMemoBy((props, _) => (props.magnitudes, props.disabled)) { (_, _) => // Memo cols
        { case (magnitudes, disabled) =>
          val deleteFn: View[Magnitude] => Callback =
            mag => magnitudes.mod(_.filterNot(_.band === mag.get.band))

          val excludeFn: View[Magnitude] => Set[MagnitudeBand] =
            mag => HashSet.from(magnitudes.get.map(_.band)) - mag.get.band

          List(
            MagTable
              .Column("value", _.zoom(Magnitude.value))
              .setHeader("Value")
              .setCell(
                ReactTableHelpers
                  .editableViewColumn(
                    Magnitude.value,
                    validFormat =
                      ValidFormatInput.fromFormat(MagnitudeValue.fromString, "Invalid magnitude"),
                    changeAuditor = ChangeAuditor
                      .fromFormat(MagnitudeValue.fromString)
                      .decimal(3)
                      .allowEmpty,
                    disabled = disabled
                  )
              ),
            MagTable
              .Column("band", _.zoom(Magnitude.band))
              .setHeader("Band")
              .setCell(
                ReactTableHelpers.editableEnumViewColumn(Magnitude.band)(
                  disabled = disabled,
                  excludeFn = Some(excludeFn)
                )
              )
              .setSortByFn(_.get),
            MagTable
              .Column("system", _.zoom(Magnitude.system))
              .setHeader("System")
              .setCell(
                ReactTableHelpers.editableEnumViewColumn(Magnitude.system)(
                  disabled = disabled
                )
              ),
            MagTable
              .Column[Unit]("delete")
              .setCell(
                ReactTableHelpers.buttonViewColumn(button = deleteButton,
                                                   onClick = deleteFn,
                                                   disabled = disabled,
                                                   wrapperClass =
                                                     ExploreStyles.MagnitudesTableDeletButtonWrapper
                )
              )
          )
        }
      }
      .useMemoBy((props, _, _) => props.magnitudes)((_, _, _) => _.toListOfViews(_.band)) // Rows
      .useTableBy((_, _, cols, rows) =>
        MagTable(cols,
                 rows,
                 _.setRowIdFn(_.get.band.tag)
                   .setInitialStateFull(tableState)
        )
      )
      .render { (props, state, _, _, tableInstance) =>
        val newBandView: Option[View[MagnitudeBand]] =
          state.value.newBand.map(band =>
            ViewF(
              band,
              (
                mod,
                _
              ) =>
                SyncIO(
                  state.modState(State.newBand.some.modify(mod)).runNow()
                ) // This View will ignore Callbacks
              // $.modStateInSyncIO(State.newBand.some.modify(mod), _.newBand.map(cb).orEmpty)
            )
          )

        val footer = TableFooter(
          TableRow(
            TableHeaderCell()(^.colSpan := 4)(
              <.div(
                ExploreStyles.MagnitudesTableFooter,
                newBandView.whenDefined { view =>
                  val addMagnitude =
                    props.magnitudes.mod(list =>
                      (list :+ Magnitude(MagnitudeValue(0), view.get, view.get.magnitudeSystem))
                        .sortBy(_.band)
                    )

                  React.Fragment(
                    EnumViewSelect(
                      id = "NEW_BAND",
                      value = view,
                      exclude = state.value.usedBands,
                      clazz = ExploreStyles.FlatFormField,
                      disabled = props.disabled
                    ),
                    Button(size = Mini,
                           compact = true,
                           onClick = addMagnitude,
                           disabled = props.disabled
                    )(^.marginLeft := "5px")(
                      Icons.New
                    )
                  )

                  React.Fragment(
                    EnumViewSelect(
                      id = "NEW_BAND",
                      value = view,
                      exclude = state.value.usedBands,
                      clazz = ExploreStyles.FlatFormField,
                      disabled = props.disabled
                    ),
                    Button(size = Mini,
                           compact = true,
                           onClick = addMagnitude,
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
            ExploreStyles.MagnitudesTableSection,
            <.label("Magnitudes"),
            Segment(attached = SegmentAttached.Attached,
                    compact = true,
                    clazz = ExploreStyles.MagnitudesTableContainer
            )(
              MagTableComponent(
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
