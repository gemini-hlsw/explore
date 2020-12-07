// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import scala.collection.immutable.HashSet

import cats.effect.IO
import cats.syntax.all._
import crystal.ViewF
import crystal.react.implicits._
import eu.timepit.refined.auto._
import eu.timepit.refined.types.string.NonEmptyString
import explore.AppCtx
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.model.display._
import explore.utils._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.enum.MagnitudeBand
import lucuma.core.math.MagnitudeValue
import lucuma.core.model.Magnitude
import lucuma.core.model.Target
import lucuma.ui.forms.EnumViewSelect
import lucuma.ui.forms.FormInputEV
import lucuma.ui.optics.ChangeAuditor
import lucuma.ui.optics.ValidFormatInput
import lucuma.ui.reusability._
import monocle.macros.Lenses
import monocle.std.option.some
import react.common.ReactProps
import react.semanticui.collections.table.Table
import react.semanticui.collections.table.TableBody
import react.semanticui.collections.table.TableCell
import react.semanticui.collections.table.TableCompact
import react.semanticui.collections.table.TableFooter
import react.semanticui.collections.table.TableHeaderCell
import react.semanticui.collections.table.TableRow
import react.semanticui.elements.button.Button
import react.semanticui.elements.segment.Segment
import react.semanticui.sizes._

final case class MagnitudeForm(
  targetId:   Target.Id,
  magnitudes: View[List[Magnitude]]
) extends ReactProps[MagnitudeForm](MagnitudeForm.component)

object MagnitudeForm {
  type Props = MagnitudeForm

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

          React.Fragment(
            <.div(<.label("Magnitudes")),
            Segment(
              Table(compact = TableCompact.Very)(
                TableBody(
                  props.magnitudes.get.toTagMod { magnitude =>
                    // We are already focused in the magnitude we want
                    val getMagnitude: List[Magnitude] => Magnitude = _ => magnitude

                    def modMagnitude(mod: Magnitude => Magnitude)
                      : List[Magnitude] => List[Magnitude] =
                      list => list.modFirstWhere(_.band === magnitude.band, mod).sortBy(_.band)

                    val magnitudeView: View[Magnitude] =
                      props.magnitudes
                        .zoom[Magnitude](getMagnitude)(modMagnitude)

                    val magnitudeValueView: View[MagnitudeValue] =
                      magnitudeView.zoom(Magnitude.value)

                    TableRow(
                      TableCell(
                        <.span(
                          FormInputEV(
                            id = NonEmptyString
                              .from(magnitude.band.toString + "_VALUE")
                              .getOrElse("NEW_MAGNITUDE_VALUE"),
                            value = magnitudeValueView,
                            validFormat = ValidFormatInput.fromFormat(MagnitudeValue.fromString,
                                                                      "Invalid magnitude"
                            ),
                            changeAuditor = ChangeAuditor
                              .fromFormat(MagnitudeValue.fromString)
                              .decimal(3)
                              .allowEmpty
                          ).withMods(^.width := "80px")
                        )
                      ),
                      TableCell(
                        <.span(
                          EnumViewSelect(
                            id = magnitude.band.toString + "_BAND",
                            value = magnitudeView.zoom(Magnitude.band),
                            exclude = state.usedBands - magnitude.band
                          )
                        )
                      ),
                      TableCell(
                        <.span(
                          EnumViewSelect(
                            id = magnitude.band.toString + "_SYSTEM",
                            value = magnitudeView.zoom(Magnitude.system)
                          )
                        )
                      ),
                      TableCell(
                        <.span(
                          Button(
                            size = Small,
                            compact = true,
                            clazz = ExploreStyles.DeleteButton,
                            onClick = props.magnitudes
                              .mod(_.filterNot(_.band === magnitude.band))
                              .runAsyncCB
                          )(
                            Icons.Delete
                              .size(Small)
                              .fitted(true)
                              .clazz(ExploreStyles.TrashIcon)
                          )
                        )
                      )
                    )
                  }
                ),
                TableFooter(
                  TableRow(
                    TableHeaderCell()(^.colSpan := 4)(
                      <.span(^.display.flex, ^.justifyContent.flexEnd)(
                        newBandView.whenDefined {
                          view =>
                            val addMagnitude =
                              props.magnitudes.mod(list =>
                                (list :+ Magnitude(MagnitudeValue(0),
                                                   view.get,
                                                   view.get.magnitudeSystem
                                )).sortBy(_.band)
                              )

                            React.Fragment(
                              EnumViewSelect(
                                id = "NEW_BAND",
                                value = view,
                                exclude = state.usedBands,
                                clazz = ExploreStyles.FlatFormField
                              ),
                              Button(size = Mini,
                                     compact = true,
                                     onClick = addMagnitude.runAsyncCB
                              )(^.marginLeft := "5px")(
                                Icons.New.size(Small).fitted(true)
                              )
                            )
                        }
                      )
                    )
                  )
                )
              )
            )(^.display.`inline-block`)
          )
        }
      }
      .configure(Reusability.shouldComponentUpdate)
      .build
}
