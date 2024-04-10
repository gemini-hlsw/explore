// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.visualization

import cats.syntax.all.*
import explore.Icons
import explore.components.ui.ExploreStyles
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.svg_<^.*
import lucuma.ags.AgsAnalysis
import lucuma.core.enums.GuideSpeed
import lucuma.react.common.Css
import lucuma.react.common.ReactFnProps
import lucuma.react.floatingui.*
import lucuma.react.floatingui.hooks.*
import lucuma.ui.syntax.all.given
import org.scalajs.dom.svg.SVG

case class GuideStarTarget(
  svg:      Option[SVG],
  offP:     Double,
  offQ:     Double,
  maxP:     Long,
  radius:   Double,
  pointCss: Css,
  sx:       Double,
  sy:       Double,
  analysis: AgsAnalysis
) extends ReactFnProps(GuideStarTarget.component)

object GuideStarTarget:
  private type Props = GuideStarTarget

  extension (s: String)
    def toSentenceCase: String =
      s"${s.charAt(0).toUpper}${s.substring(1)}"

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useState(TooltipState.Closed)
      .useFloatingBy { (_, open) =>
        UseFloatingProps(
          placement = Placement.Top,
          open = open.value.value,
          onOpenChange = s => open.setState(TooltipState(s)),
          middleware = List(
            middleware.flip()
          )
        )
      }
      .useInteractionsBy { (_, _, h) =>
        List(middleware.useHover(h.context))
      }
      .render: (p, open, floating, _) =>
        val pointCss =
          ExploreStyles.TargetTooltipArea |+| ExploreStyles.GuideStarCandidateTarget |+| p.pointCss

        val targetSvg = <.circle(^.cx := scale(p.offP),
                                 ^.cy := scale(p.offQ),
                                 ^.r  := scale(p.maxP * p.radius),
                                 ^.untypedRef(floating.refs.setReference),
                                 pointCss
        )

        if (open.value.value) {
          val ID = s"Gaia DR3 ${p.analysis.target.id}"

          val mag = p.analysis.target.gBrightness.foldMap(g => f"{g._1.shortName}: ${g._2}%.2f")

          val (translateBoxY, translateTextX, translateTextY, path, pf) =
            tooltipTranslationAndContent(floating, p.offQ, p.sx, p.sy, ID)

          def speedIcon(guideSpeed: GuideSpeed) = guideSpeed match
            case GuideSpeed.Fast   =>
              Icons.CircleSmall.withClass(ExploreStyles.AgsFast)
            case GuideSpeed.Medium =>
              Icons.CircleSmall.withClass(ExploreStyles.AgsMedium)
            case GuideSpeed.Slow   =>
              Icons.CircleSmall.withClass(ExploreStyles.AgsSlow)

          val (guideSpeedIcon, speedText) = p.analysis match
            case AgsAnalysis.Usable(_, _, guideSpeed, _, _)                    =>
              (speedIcon(guideSpeed).some, guideSpeed.tag.toSentenceCase)
            case AgsAnalysis.NotReachableAtPosition(_, _, Some(guideSpeed), _) =>
              (speedIcon(guideSpeed).some, guideSpeed.tag.toSentenceCase)
            case _                                                             =>
              (none, "")

          val (speedWidth, _) = textDomSize(speedText)
          val scaleTr         = s"scale(${1 / pf}, ${1 / pf})"
          val tooltip         =
            <.g(
              <.text(
                ^.transform := s"translate($translateTextX, ${1.5 * translateTextY}) $scaleTr",
                ID
              ),
              <.text(
                ^.transform := s"translate(${-30 / pf}, ${0.7 * translateTextY}) $scaleTr",
                mag
              ),
              <.g(
                ^.transform := s"translate(${5 / pf}, ${1.55 * translateTextY}) scale(0.02, 0.02)",
                guideSpeedIcon
              ),
              <.text(
                ^.transform := s"translate(${15 / pf}, ${0.7 * translateTextY}) $scaleTr",
                speedText
              )
            )

          <.g(
            targetSvg,
            <.g(
              ExploreStyles.TargetTooltip,
              ^.transform := s"translate(${scale(p.offP)}, ${translateBoxY}) scale($TooltipScaleFactor, $TooltipScaleFactor)",
              <.path(
                ^.untypedRef(floating.refs.setFloating),
                ^.d := path
              ),
              tooltip
            )
          )
        } else {
          targetSvg
        }
