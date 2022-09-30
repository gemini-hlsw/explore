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
import react.primereact.Dropdown
import react.primereact.SelectItem

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
          b <- t.brightestAt(w.value)
        yield b
        r.orElse(props.scienceData.flatMap(_.itcTargets.headOption))
      }(props => t => props.selectedTarget.set(t))
      .useState(Pot.pending[Map[ItcTarget, ItcChartResult]])
      // Request ITC graph data and extract ccds info from there
      .useEffectWithDepsBy((props, _) => props.queryProps) { (props, charts) => _ =>
        import props.given
        props.requestITCData(
          m =>
            charts.modStateAsync {
              case Pot.Ready(r) => Pot.Ready(r + (m.target -> m))
              case u            => Pot.Ready(Map(m.target -> m))
            },
          charts
            .setState(Pot.error(new RuntimeException("Not enough information to call ITC")))
            .to[IO],
          IO.unit
        )
      }
      .render { (props, results) =>
        def newSelected(p: ItcTarget): Option[ItcTarget] =
          props.targets.find(_ === p)

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
          Dropdown(
            clazz = ExploreStyles.ItcTileTargetSelector,
            value = props.selectedTarget.get.orUndefined,
            onChange = {
              case t @ ItcTarget(_, _, _) => props.selectedTarget.set(newSelected(t))
              case _                      => Callback.empty
            },
            options = itcTargets.map(t => SelectItem(label = t.name.value, value = t))
          ).when(itcTargets.length > 1),
          <.span(props.selectedTarget.get.map(_.name.value).getOrElse("-"))
            .when(itcTargets.length === 1),
          <.label(s"S/N per exposure:"),
          <.span(singleSN),
          <.label(s"S/N Total:"),
          <.span(totalSN)
        )
      }
