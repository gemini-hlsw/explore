// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.Eq
import cats.syntax.all._
import eu.timepit.refined.auto._
import explore._
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.targeteditor.SkyPlotNight.TimeDisplay
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.enum.Site
import lucuma.core.math.Coordinates
import lucuma.core.model.Semester
import lucuma.ui.reusability._
import react.common.ReactFnProps
import react.datepicker._
import react.semanticui.collections.form.Form
import react.semanticui.elements.button.Button
import react.semanticui.elements.button.ButtonGroup

import java.time.ZonedDateTime

final case class SkyPlotSection(
  coords:           Coordinates
)(implicit val ctx: AppContextIO)
    extends ReactFnProps[SkyPlotSection](SkyPlotSection.component)

object SkyPlotSection {
  type Props = SkyPlotSection

  protected sealed trait PlotPeriod
  protected object PlotPeriod {
    final case object Night    extends PlotPeriod
    final case object Semester extends PlotPeriod

    implicit val PlotPeriodEq: Eq[PlotPeriod]             = Eq.fromUniversalEquals
    implicit val PlotPeriodReuse: Reusability[PlotPeriod] = Reusability.byRef
  }

  implicit val propsReuse: Reusability[Props] = Reusability.derive

  val component =
    ScalaFnComponent
      .withHooks[Props]
      .useState[Site](Site.GS)
      .useState(ZonedDateTime.now(Site.GS.timezone).toLocalDate.plusDays(1))
      .useState[PlotPeriod](PlotPeriod.Night)
      .useState[TimeDisplay](TimeDisplay.Site)
      .renderWithReuse { (props, site, date, plotPeriod, timeDisplay) =>
        implicit val ctx = props.ctx

        <.div(ExploreStyles.SkyPlotSection)(
          HelpIcon("target/main/elevation-plot.md", ExploreStyles.HelpIconFloating),
          <.div(ExploreStyles.SkyPlot) {
            plotPeriod.value match {
              case PlotPeriod.Night    =>
                SkyPlotNight(site.value, props.coords, date.value, timeDisplay.value)
              case PlotPeriod.Semester =>
                val coords   = props.coords
                val semester = Semester.fromLocalDate(date.value)
                SkyPlotSemester(site.value, coords, semester).withKey(
                  s"$site-$coords-$semester"
                )
            }
          },
          Form(clazz = ExploreStyles.SkyPlotControls)(
            ButtonGroup(compact = true)(
              Button(
                active = site.value === Site.GN,
                onClick = site.setState(Site.GN)
              )("GN"),
              Button(
                active = site.value === Site.GS,
                onClick = site.setState(Site.GS)
              )("GS")
            ),
            <.div(ExploreStyles.SkyPlotDatePickerControls)(
              Button(onClick = date.modState(_.minusDays(1)),
                     clazz = ExploreStyles.SkyPlotDateButton
              )(Icons.ChevronLeftLight),
              Datepicker(
                onChange = (newValue, _) => date.setState(newValue.toLocalDateOpt.get)
              )
                .selected(date.value.toJsDate)
                .dateFormat("yyyy-MM-dd")
                .className(ExploreStyles.SkyPlotDatePicker.htmlClass),
              Button(onClick = date.modState(_.plusDays(1)),
                     clazz = ExploreStyles.SkyPlotDateButton
              )(Icons.ChevronRightLight)
            ),
            ButtonGroup(compact = true)(
              Button(
                active = plotPeriod.value === PlotPeriod.Night,
                onClick = plotPeriod.setState(PlotPeriod.Night)
              )("Night"),
              Button(
                active = plotPeriod.value === PlotPeriod.Semester,
                onClick = plotPeriod.setState(PlotPeriod.Semester)
              )("Semester")
            ),
            ButtonGroup(compact = true)(
              Button(
                active = timeDisplay.value === TimeDisplay.UTC,
                onClick = timeDisplay.setState(TimeDisplay.UTC)
              )("UT"),
              Button(
                active = timeDisplay.value === TimeDisplay.Sidereal,
                onClick = timeDisplay.setState(TimeDisplay.Sidereal)
              )("Sidereal"),
              Button(
                active = timeDisplay.value === TimeDisplay.Site,
                onClick = timeDisplay.setState(TimeDisplay.Site)
              )("Site")
            )((^.visibility.hidden.when(plotPeriod.value === PlotPeriod.Semester)))
          )
        )
      }
}
