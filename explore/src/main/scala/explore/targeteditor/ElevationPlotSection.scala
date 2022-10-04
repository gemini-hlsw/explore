// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.effect.IO
import cats.syntax.all.*
import clue.TransactionalClient
import crystal.Pot
import crystal.implicits.*
import crystal.react.View
import crystal.react.hooks.*
import crystal.react.implicits.*
import eu.timepit.refined.auto.*
import explore.*
import explore.common.UserPreferencesQueries.*
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.CoordinatesAtVizTime
import explore.model.ElevationPlotOptions
import explore.model.ScienceMode
import explore.model.enums.PlotRange
import explore.model.enums.TimeDisplay
import explore.model.reusability.*
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.Site
import lucuma.core.math.Coordinates
import lucuma.core.model.Semester
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.refined.*
import lucuma.ui.reusability.*
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import org.typelevel.log4cats.Logger
import queries.common.UserPreferencesQueriesGQL.*
import queries.schemas.UserPreferencesDB
import react.common.ReactFnProps
import react.datepicker.*
import react.semanticui.collections.form.Form
import react.semanticui.elements.button.Button
import react.semanticui.elements.button.ButtonGroup

import java.time.*

case class ElevationPlotSection(
  uid:               User.Id,
  tid:               Target.Id,
  scienceMode:       Option[ScienceMode],
  visualizationTime: Option[Instant],
  coords:            CoordinatesAtVizTime
) extends ReactFnProps(ElevationPlotSection.component)

object ElevationPlotSection:
  private type Props = ElevationPlotSection

  private given Reusability[Props] =
    Reusability.by(x => (x.uid, x.tid, x.scienceMode, x.visualizationTime, x.coords.value))

  private val preferredSiteFor = (c: Props) =>
    c.scienceMode
      .map {
        case ScienceMode.GmosNorthLongSlit(_, _) => Site.GN
        case ScienceMode.GmosSouthLongSlit(_, _) => Site.GS
      }
      .getOrElse {
        if (c.coords.value.dec.toAngle.toSignedDoubleDegrees > -5) Site.GN else Site.GS
      }

  private def prefsSetter(
    props:   Props,
    options: View[Pot[ElevationPlotOptions]],
    range:   PlotRange => PlotRange = identity,
    time:    TimeDisplay => TimeDisplay = identity
  )(using TransactionalClient[IO, UserPreferencesDB], Logger[IO]): Callback =
    options.get.toOption.map { opts =>
      ElevationPlotPreference
        .updatePlotPreferences[IO](props.uid, range(opts.range), time(opts.time))
        .runAsync
        .void
    }.getOrEmpty

  private val sitePrism = Pot.readyPrism.andThen(ElevationPlotOptions.site)

  private inline def calcTime(visualizationTime: Option[Instant], site: Site): LocalDate =
    visualizationTime
      .map(LocalDateTime.ofInstant(_, site.timezone).toLocalDate)
      .getOrElse(ZonedDateTime.now(site.timezone).toLocalDate.plusDays(1))

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useStateBy[Site]((props, _) => preferredSiteFor(props))
      // plot options, will be read from the user preferences
      .useStateView(Pot.pending[ElevationPlotOptions])
      .useEffectWithDepsBy((props, _, _, _) => props)((_, ctx, s, options) =>
        props =>
          import ctx.given

          s.setState(preferredSiteFor(props)) >>
            options.modCB(
              sitePrism.replace(preferredSiteFor(props)),
              _.map(o => prefsSetter(props, options)).toOption.getOrEmpty
            )
      )
      .useEffectWithDepsBy((props, _, _, _) => (props.uid, props.tid)) {
        (props, ctx, site, options) => _ =>
          import ctx.given

          ElevationPlotPreference
            .queryWithDefault[IO](props.uid)
            .flatMap { case (range, time) =>
              options
                .set(
                  ElevationPlotOptions.Default
                    .copy(site = site.value, range = range, time = time)
                    .ready
                )
                .to[IO]
            }
            .runAsyncAndForget
      }
      // Actual date
      .useStateBy((props, _, site, _) => calcTime(props.visualizationTime, site.value))
      // Update date if props change
      .useEffectWithDepsBy((props, _, site, _, _) => (props.visualizationTime, site.value)) {
        (_, _, _, _, date) => (vizTime, site) => date.setState(calcTime(vizTime, site))
      }
      .render { (props, ctx, _, options, date) =>
        import ctx.given

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
          siteView.set(site)

        val renderPlot: ElevationPlotOptions => VdomNode =
          (opt: ElevationPlotOptions) =>
            <.div(ExploreStyles.ElevationPlotSection)(
              HelpIcon("target/main/elevation-plot.md".refined, ExploreStyles.HelpIconFloating),
              <.div(ExploreStyles.ElevationPlot) {
                (siteView.get, rangeView.get).mapN[VdomNode] {
                  case (site, PlotRange.Night)    =>
                    ElevationPlotNight(site, props.coords, date.value, opt.time)
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
