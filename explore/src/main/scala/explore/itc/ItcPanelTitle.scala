// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.itc

import cats.effect.IO
import cats.syntax.all.*
import crystal.Pot
import crystal.react.View
import crystal.react.hooks.*
import crystal.react.implicits.*
import eu.timepit.refined.*
import eu.timepit.refined.numeric.Positive
import explore.common.ObsQueries.*
import explore.components.ui.ExploreStyles
import explore.events.*
import explore.implicits.*
import explore.model.ScienceMode
import explore.model.WorkerClients.*
import explore.model.boopickle.ItcPicklers.given
import explore.model.itc.ItcChartExposureTime
import explore.model.itc.ItcChartResult
import explore.model.itc.ItcTarget
import explore.model.itc.math.*
import explore.model.reusability.*
import explore.model.reusability.given
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.ui.reusability.*
import lucuma.ui.syntax.all.given
import queries.schemas.itc.implicits.*
import react.common.ReactFnProps
import react.semanticui.addons.select.Select
import react.semanticui.addons.select.Select.SelectItem
import react.semanticui.modules.dropdown.Dropdown.DropdownProps

import scala.scalajs.js.JSConverters._

case class ItcPanelTitle(
  scienceMode:              Option[ScienceMode],
  spectroscopyRequirements: Option[SpectroscopyRequirementsData],
  scienceData:              Option[ScienceData],
  exposure:                 Option[ItcChartExposureTime],
  selectedTarget:           View[Option[ItcTarget]]
)(using val ctx:            AppContextIO)
    extends ReactFnProps(ItcPanelTitle.component)
    with ItcPanelProps(scienceMode, spectroscopyRequirements, scienceData, exposure)

object ItcPanelTitle:
  private type Props = ItcPanelTitle with ItcPanelProps

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useEffectWithDepsBy { props =>
        val r = for
          w <- props.wavelength
          s <- props.scienceData
          t  = s.itcTargets
          b <- t.brightestAt(w)
        yield b
        r.orElse(props.scienceData.flatMap(_.itcTargets.headOption))
      }(props => t => props.selectedTarget.set(t))
      .useState(Pot.pending[Map[ItcTarget, ItcChartResult]])
      // loading
      .useState(PlotLoading.Done)
      // Request ITC graph data
      .useEffectWithDepsBy((props, _, _) =>
        (props.wavelength,
         props.scienceData.map(_.constraints),
         props.itcTargets,
         props.instrumentRow,
         props.chartExposureTime
        )
      ) {
        (props, charts, loading) =>
          (wavelength, constraints, itcTargets, instrumentRow, exposureTime) =>
            import props.given

            val action: Option[IO[Unit]] =
              for
                w           <- wavelength
                ex          <- exposureTime
                exposures   <- refineV[Positive](ex.count.value).toOption
                constraints <- constraints
                t           <- itcTargets
                mode        <- instrumentRow
              yield loading.setState(PlotLoading.Loading).to[IO] *>
                ItcClient[IO]
                  .request(
                    ItcMessage.GraphQuery(w, ex.time, exposures, constraints, t, mode)
                  )
                  .use(
                    _.evalMap(m =>
                      charts.modStateAsync {
                        case Pot.Ready(r) => Pot.Ready(r + (m.target -> m))
                        case u            => Pot.Ready(Map(m.target -> m))
                      } *> loading.setState(PlotLoading.Done).to[IO]
                    ).compile.drain
                  )
            action.getOrElse(
              (charts
                .setState(Pot.error(new RuntimeException("Not enough information to call ITC"))) *>
                loading
                  .setState(PlotLoading.Done))
                .to[IO]
            )
      }
      .render { (props, results, loading) =>
        def newSelected(p: DropdownProps): Option[ItcTarget] =
          props.targets.find(t => p.value.toOption.exists(_.toString === t.name.value))

        val selectedResult: Option[ItcChartResult] =
          props.selectedTarget.get.flatMap(t => results.value.toOption.flatMap(_.get(t)))

        val selected = props.selectedTarget.get.map(_.name.value)

        val itcTargets = props.itcTargets.foldMap(_.toList)
        val ccds       = selectedResult.map(_._2)
        val singleSN   = formatCcds(ccds, _.maxSingleSNRatio.toString)
        val totalSN    = formatCcds(ccds, _.maxTotalSNRatio.toString)

        <.div(
          ExploreStyles.ItcTileTitle,
          <.label(s"Target:"),
          Select(
            clazz = ExploreStyles.ItcTileTargetSelector,
            compact = true,
            value = selected.orUndefined,
            onChange = e => props.selectedTarget.set(newSelected(e)),
            options = itcTargets.map(t =>
              new SelectItem(text = t.name.value,
                             value = t.name.value,
                             selected = props.selectedTarget.get.exists(_ === t)
              )
            )
          ).when(itcTargets.length > 1),
          <.span(props.selectedTarget.get.map(_.name.value).getOrElse("-"))
            .when(itcTargets.length === 1),
          <.label(s"S/N per exposure:"),
          <.span(singleSN),
          <.label(s"S/N Total:"),
          <.span(totalSN)
        )
      }
