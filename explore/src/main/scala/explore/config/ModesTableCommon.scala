// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.data.EitherNec
import cats.syntax.all.*
import crystal.Pot
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.model.Progress
import explore.model.display.given
import explore.model.itc.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.math.SignalToNoise
import lucuma.core.syntax.all.*
import lucuma.core.util.NewBoolean
import lucuma.core.util.TimeSpan
import lucuma.react.circularprogressbar.CircularProgressbar
import lucuma.react.floatingui.Placement
import lucuma.react.floatingui.syntax.*
import lucuma.react.table.HeaderContext
import lucuma.ui.components.ThemeIcons
import lucuma.ui.syntax.all.given
import lucuma.ui.utils.*

import scala.collection.decorators.*

trait ModesTableCommon:
  protected case class TableMeta(itcProgress: Option[Progress])

  protected trait TableRowWithResult:
    val result: Pot[EitherNec[ItcTargetProblem, ItcResult]]

    lazy val totalItcTime: Option[TimeSpan] =
      result.toOption
        .collect { case Right(ItcResult.Result(e, t, _, _)) => e *| t.value }

    lazy val totalSN: Option[SignalToNoise] =
      result.toOption.collect { case Right(ItcResult.Result(_, _, _, s)) =>
        s.map(_.total.value)
      }.flatten

  protected object ScrollTo extends NewBoolean:
    inline def Scroll = True; inline def NoScroll = False

  protected enum TimeOrSNColumn:
    case Time, SN

  protected def progressingCellHeader(txt: String)(
    header: HeaderContext[?, ?, TableMeta, ?, ?, ?, ?]
  ) =
    <.div(ExploreStyles.ITCHeaderCell)(
      txt,
      header.table.options.meta
        .flatMap(_.itcProgress)
        .map(p =>
          CircularProgressbar(
            p.percentage.value.value,
            strokeWidth = 15,
            className = "explore-modes-table-itc-circular-progressbar"
          )
        )
    )

  protected def itcCell(
    c:   Pot[EitherNec[ItcTargetProblem, ItcResult]],
    col: TimeOrSNColumn
  ): VdomElement = {
    val content: TagMod = c.toOption match
      case Some(Left(errors))               =>
        if (errors.exists(_.problem === ItcQueryProblem.UnsupportedMode))
          <.span(Icons.Ban(^.color.red))
            .withTooltip(tooltip = "Mode not supported", placement = Placement.RightStart)
        else
          import ItcQueryProblem.*

          def renderName(name: Option[NonEmptyString]): String =
            name.fold("")(n => s"$n: ")

          val content: List[TagMod] =
            errors
              .collect:
                case ItcTargetProblem(name, s @ SourceTooBright(_)) =>
                  <.span(ThemeIcons.SunBright.addClass(ExploreStyles.ItcSourceTooBrightIcon))(
                    renderName(name) + (s: ItcQueryProblem).shortName
                  )
                case ItcTargetProblem(name, GenericError(e))        =>
                  e.split("\n")
                    .map(u => <.span(u))
                    .mkTagMod(<.span(renderName(name)), <.br, EmptyVdom)
                case ItcTargetProblem(name, problem)                =>
                  <.span(s"${renderName(name)}${problem.message}")
              .toList
              .intersperse(<.br: VdomNode)

          <.span(Icons.TriangleSolid.addClass(ExploreStyles.ItcErrorIcon))
            .withTooltip(tooltip = <.div(content.mkTagMod(<.span)), placement = Placement.RightEnd)
      case Some(Right(r: ItcResult.Result)) =>
        val content = col.match
          case TimeOrSNColumn.Time =>
            formatDurationHours(r.duration)
          case TimeOrSNColumn.SN   =>
            r.snAt.map(_.total.value).foldMap(formatSN)

        val tooltipText = col match
          case TimeOrSNColumn.Time =>
            s"${r.exposures} × ${formatDurationSeconds(r.exposureTime)}"
          case TimeOrSNColumn.SN   =>
            s"${r.snAt.map(_.single.value).foldMap(formatSN)} / exposure"

        <.span(content)
          .withTooltip(
            placement = Placement.RightStart,
            tooltip = tooltipText
          )
      case Some(Right(ItcResult.Pending))   =>
        Icons.Spinner.withSpin(true)
      case _                                =>
        "-"

    <.div(ExploreStyles.ITCCell, content)
  }
