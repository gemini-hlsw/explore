// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
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
import explore.components.ui.ExploreStyles
import explore.events.*
import explore.model.AppContext
import explore.model.BasicConfigAndItc
import explore.model.LoadingState
import explore.model.TargetList
import explore.model.WorkerClients.*
import explore.model.boopickle.ItcPicklers.given
import explore.model.display.given
import explore.model.itc.ItcChartResult
import explore.model.itc.ItcTarget
import explore.model.itc.math.*
import explore.model.reusability.given
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.syntax.display.given
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given
import lucuma.ui.syntax.pot.*
import queries.schemas.itc.syntax.*
import react.common.ReactFnProps
import react.floatingui.syntax.*
import react.primereact.Dropdown
import react.primereact.SelectItem

import scala.scalajs.js.JSConverters.*

case class ItcPanelTitle(
  selectedTarget:  View[Option[ItcTarget]],
  itcPanelProps:   ItcPanelProps,
  itcChartResults: Map[ItcTarget, Pot[ItcChartResult]],
  itcLoading:      LoadingState
) extends ReactFnProps(ItcPanelTitle.component)

object ItcPanelTitle:
  private type Props = ItcPanelTitle

  val MissingInfoMsg                   = "Not enough information to call ITC"
  val MissingInfo: Pot[ItcChartResult] =
    Pot.error(new RuntimeException(MissingInfoMsg))
  val MissingInfoIcon                  = Icons.ExclamationTriangle.withClass(ExploreStyles.WarningIcon)
  val pendingChart                     = Pot.pending[ItcChartResult]

  private val component =
    ScalaFnComponent[Props] { props =>
      def newSelected(p: Int): Option[ItcTarget] =
        props.itcPanelProps.targets.lift(p)

      val selectedResult: Pot[ItcChartResult] =
        Pot
          .fromOption(props.selectedTarget.get)
          .flatMap(t => props.itcChartResults.getOrElse(t, pendingChart))

      val selected       = props.selectedTarget.get.map(_.name.value)
      val selectedTarget = props.selectedTarget.get
      val existTargets   = props.itcPanelProps.targets.nonEmpty && selectedTarget.isDefined

      val itcTargets          = props.itcPanelProps.itcTargets.foldMap(_.toList)
      val idx                 = itcTargets.indexWhere(props.selectedTarget.get.contains)
      val itcTargetsWithIndex = itcTargets.zipWithIndex

      val ccds = selectedResult.map(_._2)

      def singleSN: ItcChartResult => VdomNode =
        (r: ItcChartResult) => <.span(formatCcds(r.ccds.some, _.maxSingleSNRatio.toString))

      def totalSN: ItcChartResult => VdomNode =
        (r: ItcChartResult) => <.span(formatSN(r.atWavelengthSNRatio.getOrElse(r.peakSNRatio)))

      def snSection(title: String, fn: ItcChartResult => VdomNode) =
        React.Fragment(
          <.label(title),
          if (existTargets && props.itcPanelProps.isExecutable) {
            selectedResult.renderPot(
              fn,
              Icons.Spinner.withSpin(true),
              e => <.span(MissingInfoIcon).withTooltip(e.getMessage)
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
