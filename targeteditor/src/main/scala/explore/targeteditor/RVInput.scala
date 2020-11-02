// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.data.ValidatedNec
import cats.effect.Async
import cats.effect.ContextShift
import cats.effect.IO
import cats.implicits._
import crystal.ViewF
import crystal.react.implicits._
import eu.timepit.refined.auto._
import explore.components.ui.ExploreStyles
import explore.model.formats._
import explore.optics._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.math.ApparentRadialVelocity
import lucuma.core.math.RadialVelocity
import lucuma.core.math.Redshift
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.ui.forms.EnumViewSelect
import lucuma.ui.forms.FormInputEV
import lucuma.ui.optics.ValidFormatInput
import lucuma.ui.reusability._
import monocle.macros.Lenses
import react.common._
import react.common.implicits._
import react.semanticui.elements.label.LabelPointing

final case class RVInput(
  value:           ViewF[IO, Option[RadialVelocity]],
  modify:          Option[RadialVelocity] => IO[Unit]
)(implicit val cs: ContextShift[IO])
    extends ReactProps[RVInput](RVInput.component)

object RVInput {
  type Props = RVInput

  sealed trait RVView extends Product with Serializable {
    def tag: String
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

    implicit val rvDisplay: Display[RVView] = Display.by(_.tag, _.tag)

  }

  @Lenses
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

  implicit def propsReuse: Reusability[Props] = Reusability.by(_.value)
  implicit def stateReuse: Reusability[State] = Reusability.derive

  class Backend($ : BackendScope[Props, State]) {
    def render(props: Props, state: State) = {
      println(s"Render $props $state")
      val rvView = ViewF.fromState[IO]($)(Async[IO], props.cs).zoom(State.rvView)
      val input  = state.rvView match {
        case RVView.Z  =>
          FormInputEV(
            id = state.rvView.tag,
            label = state.rvView.tag,
            value = props.value.zoom(unsafeRVtoZLens),
            errorClazz = ExploreStyles.InputErrorTooltip,
            errorPointing = LabelPointing.Below,
            validFormat = ValidFormatInput.fromFormatOptional(formatZ, "Must be a number"),
            onBlur = (z: ValidatedNec[String, Option[Redshift]]) =>
              z.toOption
                .map(z => props.modify(z.flatMap(_.toRadialVelocity)).runInCB)
                .getOrEmpty,
            clazz = ExploreStyles.Grow(1) |+| ExploreStyles.HideLabel
          )
        case RVView.CZ =>
          FormInputEV(
            id = state.rvView.tag,
            label = state.rvView.tag,
            value = props.value.zoom(unsafeRVtoCZLens),
            errorClazz = ExploreStyles.InputErrorTooltip,
            errorPointing = LabelPointing.Below,
            validFormat = ValidFormatInput.fromFormatOptional(formatCZ, "Must be a number"),
            onBlur = (z: ValidatedNec[String, Option[ApparentRadialVelocity]]) =>
              z.toOption
                .map(cz => props.modify(cz.map(_.toRedshift).flatMap(_.toRadialVelocity)).runInCB)
                .getOrEmpty,
            clazz = ExploreStyles.Grow(1) |+| ExploreStyles.HideLabel
          )
        case RVView.RV =>
          FormInputEV(
            id = state.rvView.tag,
            label = state.rvView.tag,
            value = props.value,
            errorClazz = ExploreStyles.InputErrorTooltip,
            errorPointing = LabelPointing.Below,
            validFormat = ValidFormatInput.fromFormatOptional(formatRV, "Must be a number"),
            onBlur = (rv: ValidatedNec[String, Option[RadialVelocity]]) =>
              rv.toOption.map(v => props.modify(v).runInCB).getOrEmpty,
            clazz = ExploreStyles.Grow(1) |+| ExploreStyles.HideLabel
          )
      }
      React.Fragment(
        <.div(
          ExploreStyles.FlexContainer |+| ExploreStyles.Grow(1),
          EnumViewSelect[IO, RVView](id = "view", value = rvView, label = state.rvView.tag),
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
