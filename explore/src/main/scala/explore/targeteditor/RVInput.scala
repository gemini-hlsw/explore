// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.syntax.all._
import crystal.react.View
import eu.timepit.refined.auto._
import eu.timepit.refined.types.string.NonEmptyString
import explore.components.ui.ExploreStyles
import explore.itc.requiredForITC
import explore.model.conversions._
import explore.model.formats._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.math.RadialVelocity
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.core.validation._
import lucuma.ui.forms.EnumViewSelect
import lucuma.ui.forms.FormInputEV
import lucuma.ui.input.ChangeAuditor
import monocle.Focus
import react.common._
import react.common.implicits._
import react.semanticui.elements.label.LabelPointing
import lucuma.refined.*

final case class RVInput(
  rv:       View[Option[RadialVelocity]],
  disabled: Boolean
) extends ReactProps[RVInput, RVInput.State, RVInput.Backend](RVInput.component)

object RVInput {
  type Props = RVInput

  sealed trait RVView extends Product with Serializable {
    def tag: NonEmptyString
  }

  object RVView {
    case object RV extends RVView {
      override val tag = "RV".refined
    }
    case object Z  extends RVView {
      override val tag = "z".refined
    }
    case object CZ extends RVView {
      override val tag = "cz".refined
    }

    implicit val rvViewEnumeration: Enumerated[RVView] =
      Enumerated.of(RV, Z, CZ)

    implicit val rvDisplay: Display[RVView] = Display.by(_.tag.value, _.tag.value)

  }

  final case class State(rvView: RVView) {
    def units(v: Option[RadialVelocity]) = rvView match {
      case RVView.Z              =>
        <.div(requiredForITC.unless(v.nonEmpty))
      case RVView.CZ | RVView.RV =>
        <.div(
          ExploreStyles.UnitsLabel,
          "km/s",
          requiredForITC.unless(v.nonEmpty)
        )
    }
  }

  object State {
    val rvView = Focus[State](_.rvView)
  }

  class Backend($ : BackendScope[Props, State]) {
    def render(props: Props, state: State) = {
      val rvView   = View.fromState($).zoom(State.rvView)
      val errorCss = ExploreStyles.InputErrorTooltip
      val baseCss  = ExploreStyles.Grow(1.refined) |+| ExploreStyles.WarningInput.when_(
        props.rv.get.isEmpty
      )
      val input    = state.rvView match {
        case RVView.Z  =>
          FormInputEV(
            id = state.rvView.tag,
            value = props.rv.zoom(rvToRedshiftGet)(rvToRedshiftMod),
            errorClazz = errorCss,
            errorPointing = LabelPointing.Below,
            validFormat =
              InputValidSplitEpi.fromFormat(formatZ, "Must be a number".refined).optional,
            changeAuditor = ChangeAuditor.fromFormat(formatZ).decimal(9.refined).optional,
            clazz = baseCss,
            disabled = props.disabled
          )
        case RVView.CZ =>
          FormInputEV(
            id = state.rvView.tag,
            value = props.rv.zoom(rvToARVGet)(rvToARVMod),
            errorClazz = errorCss,
            errorPointing = LabelPointing.Below,
            validFormat =
              InputValidSplitEpi.fromFormat(formatCZ, "Must be a number".refined).optional,
            changeAuditor = ChangeAuditor.fromFormat(formatCZ).decimal(10.refined).optional,
            clazz = baseCss,
            disabled = props.disabled
          )
        case RVView.RV =>
          FormInputEV(
            id = state.rvView.tag,
            value = props.rv,
            errorClazz = errorCss,
            errorPointing = LabelPointing.Below,
            validFormat =
              InputValidSplitEpi.fromFormat(formatRV, "Must be a number".refined).optional,
            changeAuditor = ChangeAuditor.fromFormat(formatRV).decimal(3.refined).optional,
            clazz = baseCss,
            disabled = props.disabled
          )
      }
      val label    = state.rvView match {
        case RVView.Z  =>
          state.rvView.tag.value
        case RVView.CZ =>
          state.rvView.tag.value
        case RVView.RV =>
          state.rvView.tag.value
      }
      React.Fragment(
        <.label(label, ExploreStyles.SkipToNext),
        <.div(
          ExploreStyles.FlexContainer |+| ExploreStyles.TargetRVControls,
          EnumViewSelect(id = "view", value = rvView, disabled = props.disabled),
          input
        ),
        state.units(props.rv.get)
      )
    }
  }

  val component =
    ScalaComponent
      .builder[Props]
      .initialState(State(RVView.RV))
      .renderBackend[Backend]
      .build

}
