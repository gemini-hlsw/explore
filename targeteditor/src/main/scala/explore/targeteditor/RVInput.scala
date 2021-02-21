// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.effect.IO
import cats.syntax.all._
import crystal.ViewF
import crystal.react.implicits._
import eu.timepit.refined.auto._
import eu.timepit.refined.types.string.NonEmptyString
import explore.AppCtx
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.model.formats._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.math.ApparentRadialVelocity
import lucuma.core.math.RadialVelocity
import lucuma.core.math.Redshift
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.ui.forms.EnumViewSelect
import lucuma.ui.forms.FormInputEV
import lucuma.ui.optics.ChangeAuditor
import lucuma.ui.optics.ValidFormatInput
import lucuma.ui.reusability._
import monocle.macros.Lenses
import react.common._
import react.common.implicits._
import react.semanticui.elements.label.LabelPointing

final case class RVInput(
  value:    ViewF[IO, Option[RadialVelocity]],
  disabled: ViewF[IO, Boolean]
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

  implicit def propsReuse: Reusability[Props] = Reusability.by(x => (x.value, x.disabled))
  implicit def stateReuse: Reusability[State] = Reusability.derive

  private val rvToRedshiftGet: Option[RadialVelocity] => Option[Redshift] =
    _.flatMap(_.toRedshift)

  private val rvToRedshiftMod
    : (Option[Redshift] => Option[Redshift]) => Option[RadialVelocity] => Option[RadialVelocity] =
    modZ => rv => modZ(rv.flatMap(_.toRedshift)).flatMap(_.toRadialVelocity)

  private val rvToARVGet: Option[RadialVelocity] => Option[ApparentRadialVelocity] =
    rvToRedshiftGet.andThen(_.map(_.toApparentRadialVelocity))

  private val rvToARVMod: (
    Option[ApparentRadialVelocity] => Option[ApparentRadialVelocity]
  ) => Option[RadialVelocity] => Option[RadialVelocity] =
    modZ => rvToRedshiftMod(rsOpt => modZ(rsOpt.map(_.toApparentRadialVelocity)).map(_.toRedshift))

  class Backend($ : BackendScope[Props, State]) {
    def render(props: Props, state: State) =
      AppCtx.withCtx { implicit ctx =>
        val rvView = ViewF.fromState[IO]($).zoom(State.rvView)
        val input  = state.rvView match {
          case RVView.Z  =>
            FormInputEV(
              id = state.rvView.tag,
              label = state.rvView.tag.value,
              value = props.value.zoom(rvToRedshiftGet)(rvToRedshiftMod),
              errorClazz = ExploreStyles.InputErrorTooltip,
              errorPointing = LabelPointing.Below,
              validFormat = ValidFormatInput.fromFormatOptional(formatZ, "Must be a number"),
              changeAuditor = ChangeAuditor.fromFormat(formatZ).decimal(9).optional,
              clazz = ExploreStyles.Grow(1) |+| ExploreStyles.HideLabel,
              disabled = props.disabled.get
            )
          case RVView.CZ =>
            FormInputEV(
              id = state.rvView.tag,
              label = state.rvView.tag.value,
              value = props.value.zoom(rvToARVGet)(rvToARVMod),
              errorClazz = ExploreStyles.InputErrorTooltip,
              errorPointing = LabelPointing.Below,
              validFormat = ValidFormatInput.fromFormatOptional(formatCZ, "Must be a number"),
              changeAuditor = ChangeAuditor.fromFormat(formatCZ).decimal(10).optional,
              clazz = ExploreStyles.Grow(1) |+| ExploreStyles.HideLabel,
              disabled = props.disabled.get
            )
          case RVView.RV =>
            FormInputEV(
              id = state.rvView.tag,
              label = state.rvView.tag.value,
              value = props.value,
              errorClazz = ExploreStyles.InputErrorTooltip,
              errorPointing = LabelPointing.Below,
              validFormat = ValidFormatInput.fromFormatOptional(formatRV, "Must be a number"),
              changeAuditor = ChangeAuditor.fromFormat(formatRV).decimal(3).optional,
              clazz = ExploreStyles.Grow(1) |+| ExploreStyles.HideLabel,
              disabled = props.disabled.get
            )
        }
        React.Fragment(
          <.div(
            ExploreStyles.FlexContainer |+| ExploreStyles.Grow(1),
            EnumViewSelect[IO, RVView](id = "view",
                                       value = rvView,
                                       label = state.rvView.tag.value,
                                       disabled = props.disabled.get
            ),
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
