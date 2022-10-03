// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.itc

import cats.effect.IO
import cats.syntax.all.*
import crystal.Pot
import crystal.implicits.*
import crystal.react.View
import crystal.react.hooks.*
import crystal.react.implicits.*
import eu.timepit.refined.*
import eu.timepit.refined.numeric.Positive
import explore.Icons
import explore.common.ObsQueries.*
import explore.components.ui.ExploreStyles
import explore.events.*
import explore.model.AppContext
import explore.model.ScienceMode
import explore.model.WorkerClients.*
import explore.model.boopickle.ItcPicklers.given
import explore.model.itc.ItcChartExposureTime
import explore.model.itc.ItcChartResult
import explore.model.itc.ItcTarget
import explore.model.itc.math.*
import explore.model.reusability.*
import explore.model.reusability.given
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.ui.reusability.*
import lucuma.ui.syntax.all.given
import queries.schemas.itc.implicits.*
import react.common.ReactFnProps
import react.floatingui.syntax.*
import react.primereact.Dropdown
import react.primereact.SelectItem

import scala.scalajs.js.JSConverters.*

case class ItcPanelTitle(
  scienceMode:              Option[ScienceMode],
  spectroscopyRequirements: Option[SpectroscopyRequirementsData],
  scienceData:              Option[ScienceData],
  exposure:                 Option[ItcChartExposureTime],
  selectedTarget:           View[Option[ItcTarget]]
) extends ReactFnProps(ItcPanelTitle.component)
    with ItcPanelProps(scienceMode, spectroscopyRequirements, scienceData, exposure)

object ItcPanelTitle:
  private type Props = ItcPanelTitle with ItcPanelProps

  val MissingInfoMsg                   = "Not enough information to call ITC"
  val MissingInfo: Pot[ItcChartResult] =
    Pot.error(new RuntimeException(MissingInfoMsg))
  val MissingInfoIcon                  = Icons.ExclamationTriangle.clazz(ExploreStyles.WarningIcon)

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useEffectWithDepsBy { (props, _) =>
        val r = for
          w <- props.wavelength
          s <- props.scienceData
          t  = s.itcTargets
          b <- t.brightestAt(w.value)
        yield b
        r.orElse(props.scienceData.flatMap(_.itcTargets.headOption))
      }((props, _) => t => props.selectedTarget.set(t))
      .useStateBy((props, _) =>
        props.itcTargets.map(_.toList.map(t => t -> MissingInfo).toMap).getOrElse(Map.empty)
      )
      // Request ITC graph data and extract ccds info from there
      .useEffectWithDepsBy((props, _, _) => props.queryProps) { (props, ctx, charts) => _ =>
        import ctx.given

        props.requestITCData(
          m =>
            charts.modStateAsync { r =>
              r + (m.target -> m.ready)
            },
          charts
            .setState(
              props.itcTargets.foldMap(_.toList.map(t => t -> MissingInfo)).toMap
            )
            .to[IO],
          IO.unit
        )
      }
      .render { (props, _, results) =>
        def newSelected(p: Int): Option[ItcTarget] =
          props.targets.lift(p)

        val selectedResult: Pot[ItcChartResult] =
          Pot
            .fromOption(props.selectedTarget.get)
            .flatMap(t => results.value.getOrElse(t, Pot.pending[ItcChartResult]))

        val selected       = props.selectedTarget.get.map(_.name.value)
        val selectedTarget = props.selectedTarget.get

        val itcTargets          = props.itcTargets.foldMap(_.toList)
        val idx                 = itcTargets.indexWhere(selectedTarget.contains)
        val itcTargetsWithIndex = itcTargets.zipWithIndex

        val ccds                                 = selectedResult.map(_._2)
        def singleSN: ItcChartResult => VdomNode =
          (r: ItcChartResult) => <.span(formatCcds(r.ccds.some, _.maxSingleSNRatio.toString))
        def totalSN: ItcChartResult => VdomNode  =
          (r: ItcChartResult) => <.span(formatCcds(r.ccds.some, _.maxTotalSNRatio.toString))
        val existTargets                         = props.targets.nonEmpty

        def snSection(title: String, fn: ItcChartResult => VdomNode) =
          React.Fragment(
            <.label(title),
            if (existTargets) {
              potRender[ItcChartResult](
                singleSN,
                Icons.Spinner.spin(true),
                e => <.span(MissingInfoIcon).withTooltip(e.getMessage)
              )(
                selectedResult
              )
            } else {
              <.span(MissingInfoIcon).withTooltip(MissingInfoMsg)
            }
          )

        <.div(
          ExploreStyles.ItcTileTitle,
          <.label(s"Target:"),
          Dropdown(
            clazz = ExploreStyles.ItcTileTargetSelector,
            value = idx,
            onChange = {
              case t: Int => props.selectedTarget.set(newSelected(t))
              case _      => Callback.empty
            },
            options = itcTargetsWithIndex.map((t, i) => SelectItem(label = t.name.value, value = i))
          ).when(itcTargets.length > 1),
          <.span(props.selectedTarget.get.map(_.name.value).getOrElse("-"))
            .when(itcTargets.length === 1),
          snSection("S/N per exposure:", singleSN),
          snSection("S/N Total:", totalSN)
        )
      }
