// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.effect.IO
import cats.syntax.all._
import crystal.Pot
import crystal.implicits._
import crystal.react.View
import crystal.react.hooks._
import crystal.react.implicits._
import eu.timepit.refined.auto._
import explore._
import explore.common.UserPreferencesQueries._
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.model.ElevationPlotOptions
import explore.model.ScienceMode
import explore.model.enums.PlotRange
import explore.model.enums.TimeDisplay
import explore.model.reusability._
import explore.utils._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.enums.Site
import lucuma.core.math.Coordinates
import lucuma.core.model.Semester
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.ui.reusability._
import queries.common.UserPreferencesQueriesGQL._
import react.common._
import react.datepicker._
import react.semanticui.collections.form.Form
import react.semanticui.elements.button.Button
import react.semanticui.elements.button.ButtonGroup
import lucuma.refined.*

import java.time.ZonedDateTime

final case class ElevationPlotSection(
  uid:              User.Id,
  tid:              Target.Id,
  scienceMode:      Option[ScienceMode],
  coords:           Coordinates
)(implicit val ctx: AppContextIO)
    extends ReactFnProps[ElevationPlotSection](ElevationPlotSection.component)

object ElevationPlotSection {
  type Props = ElevationPlotSection

  // implicit val propsReuse: Reusability[Props] = Reusability.derive
  implicit val propsReuse: Reusability[Props] = Reusability.never

  val preferredSiteFor = (c: Props) =>
    c.scienceMode
      .map {
        case ScienceMode.GmosNorthLongSlit(_, _) => Site.GN
        case ScienceMode.GmosSouthLongSlit(_, _) => Site.GS
      }
      .getOrElse {
        if (c.coords.dec.toAngle.toSignedDoubleDegrees > -5) Site.GN else Site.GS
      }

  def prefsSetter(
    props:        Props,
    options:      View[Pot[ElevationPlotOptions]],
    site:         Site => Site = identity,
    range:        PlotRange => PlotRange = identity,
    time:         TimeDisplay => TimeDisplay = identity
  )(implicit ctx: AppContextIO): Callback =
    options.get.toOption.map { opts =>
      UserTargetPreferencesUpsert
        .updatePlotPreferences[IO](props.uid,
                                   props.tid,
                                   site(opts.site),
                                   range(opts.range),
                                   time(opts.time)
        )
        .runAsync
        .void
    }.getOrEmpty

  val sitePrism = Pot.readyPrism.andThen(ElevationPlotOptions.site)

  val component =
    ScalaFnComponent
      .withHooks[Props]
      .useStateBy[Site](preferredSiteFor)
      // plot options, will be read from the user preferences
      .useStateView(Pot.pending[ElevationPlotOptions])
      .useEffectWithDepsBy((p, _, _) => p)((_, s, options) =>
        p =>
          s.setState(preferredSiteFor(p)) *>
            options.modCB(
              sitePrism.replace(preferredSiteFor(p)),
              _.map(o => prefsSetter(p, options, site = _ => o.site)(p.ctx)).toOption.getOrEmpty
            )
      )
      .useEffectWithDepsBy((p, _, _) => (p.uid, p.tid)) { (props, site, options) => _ =>
        implicit val ctx = props.ctx
        UserElevationPlotPreferencesQuery
          .queryWithDefault[IO](props.uid, props.tid, site.value)
          .flatMap { case (site, range, time) =>
            options
              .set(
                ElevationPlotOptions.Default
                  .copy(site = site, range = range, time = time)
                  .ready
              )
              .to[IO]
          }
          .runAsyncAndForget
      }
      // Actual date
      .useState(ZonedDateTime.now(Site.GS.timezone).toLocalDate.plusDays(1))
      .render { (props, _, options, date) =>
        implicit val ctx = props.ctx

        val siteView =
          options.zoom(sitePrism)

        val timeView =
          options.zoom(Pot.readyPrism.andThen(ElevationPlotOptions.time))

        val rangeView =
          options.zoom(Pot.readyPrism.andThen(ElevationPlotOptions.range))

        def setTime(timeDisplay: TimeDisplay) =
          timeView.set(timeDisplay) *> prefsSetter(props, options, time = _ => timeDisplay)

        def setRange(timeRange: PlotRange) =
          rangeView.set(timeRange) *> prefsSetter(props, options, range = _ => timeRange)

        def setSite(site: Site) =
          siteView.set(site) *> prefsSetter(props, options, site = _ => site)

        val renderPlot: ElevationPlotOptions => VdomNode =
          (opt: ElevationPlotOptions) =>
            <.div(ExploreStyles.ElevationPlotSection)(
              HelpIcon("target/main/elevation-plot.md".refined, ExploreStyles.HelpIconFloating),
              <.div(ExploreStyles.ElevationPlot) {
                (siteView.get, rangeView.get).mapN[VdomNode] {
                  case (site, PlotRange.Night)    =>
                    <.div()
                  // ElevationPlotNight(site, props.coords, date.value, opt.time)
                  case (site, PlotRange.Semester) =>
                    val coords   = props.coords
                    val semester = Semester.fromLocalDate(date.value)
                    ElevationPlotSemester(site, coords, semester).withKey(
                      s"${siteView.get}-$coords-$semester"
                    )
                  case _                          => EmptyVdom
                }
              },
              Form(clazz = ExploreStyles.ElevationPlotControls)(
                ButtonGroup(compact = true)(
                  Button(
                    active = siteView.contains(Site.GN),
                    onClick = setSite(Site.GN)
                  )("GN"),
                  Button(
                    active = siteView.contains(Site.GS),
                    onClick = setSite(Site.GS)
                  )("GS")
                ),
                <.div(ExploreStyles.ElevationPlotDatePickerControls)(
                  Button(onClick = date.modState(_.minusDays(1)),
                         clazz = ExploreStyles.ElevationPlotDateButton
                  )(Icons.ChevronLeftLight),
                  Datepicker(
                    onChange = (newValue, _) => date.setState(newValue.toLocalDateOpt.get)
                  )
                    .selected(date.value.toJsDate)
                    .dateFormat("yyyy-MM-dd")
                    .className(ExploreStyles.ElevationPlotDatePicker.htmlClass),
                  Button(onClick = date.modState(_.plusDays(1)),
                         clazz = ExploreStyles.ElevationPlotDateButton
                  )(Icons.ChevronRightLight)
                ),
                ButtonGroup(compact = true)(
                  Button(
                    active = rangeView.contains(PlotRange.Night),
                    onClick = setRange(PlotRange.Night)
                  )("Night"),
                  Button(
                    active = rangeView.contains(PlotRange.Semester),
                    onClick = setRange(PlotRange.Semester)
                  )("Semester")
                ),
                ButtonGroup(compact = true)(
                  Button(
                    active = timeView.contains(TimeDisplay.UT),
                    onClick = setTime(TimeDisplay.UT)
                  )("UT"),
                  Button(
                    active = timeView.contains(TimeDisplay.Sidereal),
                    onClick = setTime(TimeDisplay.Sidereal)
                  )("Sidereal"),
                  Button(
                    active = timeView.contains(TimeDisplay.Site),
                    onClick = setTime(TimeDisplay.Site)
                  )("Site")
                )(^.visibility.hidden.when(rangeView.contains(PlotRange.Semester)))
              )
            )

        potRenderView[ElevationPlotOptions](renderPlot)(options)
      }
}
