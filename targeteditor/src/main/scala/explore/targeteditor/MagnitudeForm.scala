// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.effect.IO
import cats.syntax.all._
import crystal.ViewF
import crystal.react.implicits._
import eu.timepit.refined.auto._
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.model.display._
import explore.utils.ReactTableHelpers
import explore.{ AppCtx, Icons }
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.enum.MagnitudeBand
import lucuma.core.math.MagnitudeValue
import lucuma.core.model.{ Magnitude, Target }
import lucuma.ui.forms.EnumViewSelect
import lucuma.ui.optics.{ ChangeAuditor, ValidFormatInput }
import lucuma.ui.reusability._
import monocle.macros.Lenses
import monocle.std.option.some
import react.common.{ Css, ReactProps }
import react.semanticui.collections.table.{ TableFooter, TableHeaderCell, TableRow }
import react.semanticui.elements.button.Button
import react.semanticui.elements.segment.Segment
import react.semanticui.sizes._
import reactST.reactTable.TableMaker
import reactST.reactTable.mod._

import scala.collection.immutable.HashSet

import scalajs.js.JSConverters._
import react.semanticui.elements.segment.SegmentAttached

final case class MagnitudeForm(
  targetId:   Target.Id,
  magnitudes: View[List[Magnitude]],
  disabled:   Boolean
) extends ReactProps[MagnitudeForm](MagnitudeForm.component)

object MagnitudeForm {
  type Props = MagnitudeForm

  sealed trait Column extends Product with Serializable {
    def id: String
    def label: String
  }
  object Column {
    case object Value  extends Column {
      val id    = "value"
      val label = id
    }
    case object Band   extends Column {
      val id    = "band"
      val label = id
    }
    case object System extends Column {
      val id    = "system"
      val label = id
    }
    case object Delete extends Column {
      val id    = "delete"
      val label = ""
    }

    val all: Map[String, Column] = List(Value, Band, System, Delete).fproductLeft(_.id).toMap
  }

  @Lenses
  protected case class State(usedBands: Set[MagnitudeBand], newBand: Option[MagnitudeBand])

  implicit val propsReuse: Reusability[Props] = Reusability.derive
  implicit val stateReuse: Reusability[State] = Reusability.derive

  val component =
    ScalaComponent
      .builder[Props]
      .getDerivedStateFromPropsAndState[State] { case (props, stateOpt) =>
        val usedBands = HashSet.from(props.magnitudes.get.map(_.band))
        stateOpt match {
          case Some(state) if state.newBand.exists(b => !usedBands.contains(b)) =>
            State.usedBands.set(usedBands)(state)
          case _                                                                =>
            State(usedBands, MagnitudeBand.all.diff(usedBands.toList).headOption)
        }
      }
      .render { $ =>
        val props = $.props
        val state = $.state

        AppCtx.withCtx { implicit ctx =>
          val newBandView: Option[View[MagnitudeBand]] =
            state.newBand.map(band =>
              ViewF(band,
                    (mod: MagnitudeBand => MagnitudeBand) =>
                      $.modStateIn[IO](State.newBand.composePrism(some).modify(mod))
              )
            )

          val deleteButton = Button(
            size = Small,
            compact = true,
            clazz = ExploreStyles.DeleteButton
          )(
            Icons.Delete
              .size(Small)
              .fitted(true)
              .clazz(ExploreStyles.TrashIcon)
          )

          val deleteFn: View[Magnitude] => Callback =
            mag => props.magnitudes.mod(_.filterNot(_.band === mag.get.band)).runAsyncCB

          val excludeFn: View[Magnitude] => Set[MagnitudeBand] =
            mag => HashSet.from(props.magnitudes.get.map(_.band)) - mag.get.band

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
                        exclude = state.usedBands,
                        clazz = ExploreStyles.FlatFormField,
                        disabled = props.disabled
                      ),
                      Button(size = Mini,
                             compact = true,
                             onClick = addMagnitude.runAsyncCB,
                             disabled = props.disabled
                      )(^.marginLeft := "5px")(
                        Icons.New.size(Small).fitted(true)
                      )
                    )
                  }
                )
              )
            )
          )

          val tableMaker = TableMaker[View[Magnitude]].withSort
          import tableMaker.syntax._

          val columns = tableMaker.columnArray(
            tableMaker
              .componentColumn(
                Column.Value.id,
                ReactTableHelpers
                  .editableViewColumn(
                    Magnitude.value,
                    validFormat =
                      ValidFormatInput.fromFormat(MagnitudeValue.fromString, "Invalid magnitude"),
                    changeAuditor = ChangeAuditor
                      .fromFormat(MagnitudeValue.fromString)
                      .decimal(3)
                      .allowEmpty,
                    disabled = props.disabled
                  )
              ),
            tableMaker
              .componentColumn(Column.Band.id,
                               ReactTableHelpers.editableEnumViewColumn(Magnitude.band)(
                                 disabled = props.disabled,
                                 excludeFn = Some(excludeFn)
                               )
              )
              .setSortByFn(_.get.band),
            tableMaker
              .componentColumn(Column.System.id,
                               ReactTableHelpers.editableEnumViewColumn(Magnitude.system)(
                                 disabled = props.disabled
                               )
              ),
            tableMaker.componentColumn(
              Column.Delete.id,
              ReactTableHelpers.buttonViewColumn(button = deleteButton,
                                                 onClick = deleteFn,
                                                 disabled = props.disabled,
                                                 wrapperClass =
                                                   ExploreStyles.MagnitudesTableDeletButtonWrapper
              )
            )
          )

          val tableState = tableMaker.emptyState.setSortByVarargs(SortingRule("band"))
          val options    = tableMaker
            .options(rowIdFn = _.get.band.tag, columns = columns)
            .setInitialStateFull(tableState)

          React.Fragment(
            <.div(
              <.label("Magnitudes"),
              Segment(attached = SegmentAttached.Attached,
                      compact = true,
                      clazz = ExploreStyles.MagnitudesTableContainer
              )(
                tableMaker.makeTable(
                  options = options,
                  data = props.magnitudes.toListOfViews(_.band).toJSArray,
                  headerCellFn = Some(c => <.th(Column.all.get(c.id.toString).foldMap(_.label))),
                  tableClass = Css("ui very celled selectable  striped compact table"),
                  footer = footer
                )
              )
            )
          )
        }
      }
      .configure(Reusability.shouldComponentUpdate)
      .build
}
