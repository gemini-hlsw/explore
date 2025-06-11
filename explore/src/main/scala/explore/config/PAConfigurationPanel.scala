// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.syntax.all.*
import crystal.react.*
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.model.AveragePABasis
import explore.model.Observation
import explore.model.enums.AgsState
import explore.model.enums.PosAngleOptions
import explore.model.syntax.all.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.math.Angle
import lucuma.core.math.validation.MathValidators
import lucuma.core.model.PosAngleConstraint
import lucuma.core.model.Program
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Tooltip
import lucuma.refined.*
import lucuma.ui.components.TimeSpanView
import lucuma.ui.input.ChangeAuditor
import lucuma.ui.primereact.FormEnumDropdownView
import lucuma.ui.primereact.FormInputTextView
import lucuma.ui.primereact.LucumaPrimeStyles
import lucuma.ui.primereact.given
import lucuma.ui.syntax.all.given
import monocle.Lens

case class PAConfigurationPanel(
  programId:    Program.Id,
  obsId:        Observation.Id,
  posAngleView: View[PosAngleConstraint],
  selectedPA:   Option[Angle],
  averagePA:    Option[AveragePABasis],
  agsState:     View[AgsState],
  readonly:     Boolean
) extends ReactFnProps(PAConfigurationPanel.component)

object PAConfigurationPanel:
  private type Props = PAConfigurationPanel

  /**
   * Used to convert pos angle and an enumeration for a UI selector It is unsafe as the angle is
   * lost for Average Parallictic and Unconstrained
   */
  private val unsafePosOptionsLens: Lens[PosAngleConstraint, PosAngleOptions] =
    Lens[PosAngleConstraint, PosAngleOptions](_.toPosAngleOptions)((a: PosAngleOptions) =>
      (b: PosAngleConstraint) =>
        a.toPosAngle(b match {
          case PosAngleConstraint.Fixed(a)               => a
          case PosAngleConstraint.AllowFlip(a)           => a
          case PosAngleConstraint.AverageParallactic     => Angle.Angle0
          case PosAngleConstraint.ParallacticOverride(a) => a
          case PosAngleConstraint.Unbounded              => Angle.Angle0
        })
    )

  private val component =
    ScalaFnComponent[Props] { props =>
      val paView = props.posAngleView

      val posAngleOptionsView: View[PosAngleOptions] =
        paView.zoom(unsafePosOptionsLens)

      val fixedView: ViewOpt[Angle] =
        paView
          .zoom(PosAngleConstraint.fixedAngle)

      val allowedFlipView: ViewOpt[Angle] =
        paView
          .zoom(PosAngleConstraint.allowFlipAngle)

      val parallacticOverrideView: ViewOpt[Angle] =
        paView
          .zoom(PosAngleConstraint.parallacticOverrideAngle)

      val selectedAngle = props.posAngleView.get match
        case PosAngleConstraint.Unbounded                                          =>
          props.selectedPA
            .map(a => <.label(f"${a.toDoubleDegrees}%.0f °"))
        case PosAngleConstraint.AverageParallactic                                 =>
          props.averagePA
            .map: a =>
              <.div(ExploreStyles.AveragePA)(
                <.label(f"${a.averagePA.toDoubleDegrees}%.2f °"),
                <.label(a.when.toString),
                <.label(TimeSpanView(a.duration, tooltipPosition = Tooltip.Position.Left.some))
              )
            .orElse(
              <.label(
                "Not Visible, observation complete, or explicit observation duration is less than setup time."
              ).some
            )
        case PosAngleConstraint.AllowFlip(af) if props.selectedPA.exists(_ =!= af) =>
          props.selectedPA
            .map(a => <.label(f"Flipped to ${a.toDoubleDegrees}%.0f °"))
        case _                                                                     => None

      def posAngleEditor(pa: View[Angle]) =
        <.div(
          FormInputTextView(
            id = "pos-angle-value".refined,
            groupClass = ExploreStyles.PAConfigurationAngle,
            value = pa,
            units = "° E of N",
            disabled = !props.agsState.get.canRecalculate || props.readonly,
            validFormat = MathValidators.truncatedAngleDegrees,
            changeAuditor = ChangeAuditor.bigDecimal(3.refined, 2.refined)
          )(^.autoComplete.off)
        )

      <.div(
        LucumaPrimeStyles.FormColumnCompact,
        ExploreStyles.PAConfigurationForm
      )(
        FormEnumDropdownView(
          id = "pos-angle-alternative".refined,
          label =
            React.Fragment("Position Angle", HelpIcon("configuration/positionangle.md".refined)),
          value = posAngleOptionsView,
          disabled = !props.agsState.get.canRecalculate || props.readonly
        ),
        fixedView.mapValue(posAngleEditor),
        allowedFlipView.mapValue(posAngleEditor),
        parallacticOverrideView.mapValue(posAngleEditor),
        selectedAngle
      )
    }
