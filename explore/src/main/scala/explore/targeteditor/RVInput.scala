// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.syntax.all._
import crystal.ViewF
import crystal.react.implicits._
import eu.timepit.refined.auto._
import eu.timepit.refined.types.string.NonEmptyString
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.model.conversions._
import explore.model.formats._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.math.RadialVelocity
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.ui.forms.EnumViewSelect
import lucuma.ui.forms.FormInputEV
import lucuma.ui.optics.ChangeAuditor
import lucuma.ui.optics.ValidFormatInput
import lucuma.ui.reusability._
import monocle.Focus
import react.common._
import react.common.implicits._
import react.semanticui.elements.label.LabelPointing

final case class RVInput(
  value:    View[Option[RadialVelocity]],
  disabled: Boolean
) extends ReactProps[RVInput](RVInput.component)

object RVInput {
  type Props = RVInput

  sealed trait RVView extends Product with Serializable {
    def tag: NonEmptyString
  }

  object RVView {
    case object RV extends RVView {
      override val tag = "RV"
    }
    case object Z  extends RVView {
      override val tag = "z"
    }
    case object CZ extends RVView {
      override val tag = "cz"
    }

    implicit val rvViewEnumeration: Enumerated[RVView] =
      Enumerated.of(RV, Z, CZ)

    implicit val rvDisplay: Display[RVView] = Display.by(_.tag.value, _.tag.value)

  }

  final case class State(rvView: RVView) {
    val units = rvView match {
      case RVView.Z              =>
        <.div()
      case RVView.CZ | RVView.RV =>
        <.div(
          ExploreStyles.UnitsLabel,
          "km/s"
        )
    }
  }

  object State {
    val rvView = Focus[State](_.rvView)
  }

  implicit def propsReuse: Reusability[Props] = Reusability.derive
  implicit def stateReuse: Reusability[State] = Reusability.derive

  class Backend($ : BackendScope[Props, State]) {
    def render(props: Props, state: State) = {
      val rvView = ViewF.fromState($).zoom(State.rvView)
      val input  = state.rvView match {
        case RVView.Z  =>
          FormInputEV(
            id = state.rvView.tag,
            value = props.value.zoom(rvToRedshiftGet)(rvToRedshiftMod),
            errorClazz = ExploreStyles.InputErrorTooltip,
            errorPointing = LabelPointing.Below,
            validFormat = ValidFormatInput.fromFormatOptional(formatZ, "Must be a number"),
            changeAuditor = ChangeAuditor.fromFormat(formatZ).decimal(9).optional,
            clazz = ExploreStyles.Grow(1),
            disabled = props.disabled
          )
        case RVView.CZ =>
          FormInputEV(
            id = state.rvView.tag,
            value = props.value.zoom(rvToARVGet)(rvToARVMod),
            errorClazz = ExploreStyles.InputErrorTooltip,
            errorPointing = LabelPointing.Below,
            validFormat = ValidFormatInput.fromFormatOptional(formatCZ, "Must be a number"),
            changeAuditor = ChangeAuditor.fromFormat(formatCZ).decimal(10).optional,
            clazz = ExploreStyles.Grow(1),
            disabled = props.disabled
          )
        case RVView.RV =>
          FormInputEV(
            id = state.rvView.tag,
            value = props.value,
            errorClazz = ExploreStyles.InputErrorTooltip,
            errorPointing = LabelPointing.Below,
            validFormat = ValidFormatInput.fromFormatOptional(formatRV, "Must be a number"),
            changeAuditor = ChangeAuditor.fromFormat(formatRV).decimal(3).optional,
            clazz = ExploreStyles.Grow(1),
            disabled = props.disabled
          )
      }
      val label  = state.rvView match {
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
        state.units
      )
    }
  }

  val component =
    ScalaComponent
      .builder[Props]
      .initialState(State(RVView.RV))
      .renderBackend[Backend]
      .configure(Reusability.shouldComponentUpdate)
      .build

}
