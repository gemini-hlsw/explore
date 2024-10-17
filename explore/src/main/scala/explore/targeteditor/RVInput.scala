// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.syntax.all.*
import crystal.react.View
import crystal.react.hooks.*
import eu.timepit.refined.auto.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.components.ui.ExploreStyles
import explore.itc.renderRequiredForITCIcon
import explore.model.conversions.*
import explore.model.formats.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.math.RadialVelocity
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.core.validation.*
import lucuma.react.common.ReactFnProps
import lucuma.refined.*
import lucuma.ui.input.ChangeAuditor
import lucuma.ui.primereact.EnumDropdownView
import lucuma.ui.primereact.FormInputTextView
import lucuma.ui.primereact.FormLabel
import lucuma.ui.primereact.LucumaPrimeStyles
import lucuma.ui.primereact.given
import lucuma.ui.syntax.all.given

case class RVInput(
  rv:              View[Option[RadialVelocity]],
  disabled:        Boolean,
  calibrationRole: Option[CalibrationRole]
) extends ReactFnProps[RVInput](RVInput.component)

object RVInput {
  protected type Props = RVInput

  private enum RVView(val tag: NonEmptyString):
    case RV extends RVView("RV".refined)
    case Z  extends RVView("z".refined)
    case CZ extends RVView("cz".refined)

  private object RVView:
    given Enumerated[RVView] = Enumerated.from(RVView.RV, RVView.Z, RVView.CZ).withTag(_.tag)
    given Display[RVView]    = Display.byShortName(_.tag.value)

  private def addons(v: Option[RadialVelocity], role: Option[CalibrationRole]): List[TagMod] =
    if (v.isEmpty) List(role.renderRequiredForITCIcon) else List.empty

  protected val component =
    ScalaFnComponent
      .withHooks[Props]
      .useStateView[RVView](RVView.RV)
      .render { (props, rvView) =>

        val baseCss = ExploreStyles.Grow(1.refined) |+|
          ExploreStyles.WarningInput.when_(props.rv.get.isEmpty)

        val input = rvView.get match {
          case RVView.Z  =>
            FormInputTextView(
              id = rvView.get.tag,
              value = props.rv.zoom(rvToRedshiftGet)(rvToRedshiftMod),
              validFormat =
                InputValidSplitEpi.fromFormat(formatZ, "Must be a number".refined).optional,
              changeAuditor = ChangeAuditor.fromFormat(formatZ).decimal(9.refined).optional,
              groupClass = baseCss,
              disabled = props.disabled,
              postAddons = addons(props.rv.get, props.calibrationRole)
            )
          case RVView.CZ =>
            FormInputTextView(
              id = rvView.get.tag,
              value = props.rv.zoom(rvToARVGet)(rvToARVMod),
              validFormat =
                InputValidSplitEpi.fromFormat(formatCZ, "Must be a number".refined).optional,
              changeAuditor = ChangeAuditor.fromFormat(formatCZ).decimal(10.refined).optional,
              groupClass = baseCss,
              disabled = props.disabled,
              units = "km/s",
              postAddons = addons(props.rv.get, props.calibrationRole)
            )
          case RVView.RV =>
            FormInputTextView(
              id = rvView.get.tag,
              value = props.rv,
              validFormat =
                InputValidSplitEpi.fromFormat(formatRV, "Must be a number".refined).optional,
              changeAuditor = ChangeAuditor.fromFormat(formatRV).decimal(3.refined).optional,
              groupClass = baseCss,
              disabled = props.disabled,
              units = "km/s",
              postAddons = addons(props.rv.get, props.calibrationRole)
            )
        }
        React.Fragment(
          FormLabel(htmlFor = "rv-view".refined)(rvView.get.tag.value),
          <.div(
            ExploreStyles.FlexContainer |+| ExploreStyles.TargetRVControls |+| LucumaPrimeStyles.FormField,
            EnumDropdownView(id = "rv-view".refined, value = rvView, disabled = props.disabled),
            input
          )
        )
      }
}
