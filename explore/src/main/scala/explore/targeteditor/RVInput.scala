// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import algebra.instances.all.given
import cats.syntax.all.*
import coulomb.policy.spire.standard.given
import crystal.react.View
import crystal.react.hooks.*
import crystal.react.syntax.all.*
import explore.common.UserPreferencesQueries
import explore.components.ui.ExploreStyles
import explore.itc.renderRequiredForITCIcon
import explore.model.AppContext
import explore.model.UserPreferences
import explore.model.conversions.*
import explore.model.enums.LineOfSightMotion
import explore.model.formats.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.math.Constants
import lucuma.core.math.RadialVelocity
import lucuma.core.math.Redshift
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.syntax.all.*
import lucuma.core.util.Display
import lucuma.core.validation.*
import lucuma.react.common.ReactFnProps
import lucuma.refined.*
import lucuma.ui.input.ChangeAuditor
import lucuma.ui.primereact.EnumDropdownView
import lucuma.ui.primereact.FormInputTextView
import lucuma.ui.primereact.FormLabel
import lucuma.ui.primereact.LucumaPrimeStyles
import lucuma.ui.primereact.given
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given

import scala.language.implicitConversions

case class RVInput(
  rv:              View[Option[RadialVelocity]],
  disabled:        Boolean,
  calibrationRole: Option[CalibrationRole],
  targetId:        Target.Id,
  preferences:     View[UserPreferences],
  userId:          User.Id
) extends ReactFnProps[RVInput](RVInput.component)

object RVInput {
  protected type Props = RVInput

  given Display[LineOfSightMotion] = Display.byShortName:
    case LineOfSightMotion.RV => "RV"
    case LineOfSightMotion.Z  => "z"
    case LineOfSightMotion.CZ => "cz"

  private def addons(v: Option[RadialVelocity], role: Option[CalibrationRole]): List[TagMod] =
    if (v.isEmpty) List(role.renderRequiredForITCIcon) else List.empty

  // Over 1% speed of light, use Z
  val LOSLimit = Constants.SpeedOfLight.toValue[BigDecimal] * BigDecimal(0.01)

  private def defaultLOS(p: Props): LineOfSightMotion =
    p.rv.get match
      case Some(rv) if rv.rv > LOSLimit => LineOfSightMotion.Z
      case _                            => LineOfSightMotion.RV

  protected val component = ScalaFnComponent[Props]: props =>
    for {
      ctx    <- useContext(AppContext.ctx)
      rvView <- useStateView(defaultLOS(props)) // Start with default
      _      <- useEffectWithDeps((props.targetId, props.userId)): (tid, _) =>
                  // Check cache first, then database
                  props.preferences
                    .zoom(UserPreferences.targetLineOfSightMotion(tid))
                    .get match
                    case Some(los) =>
                      rvView.set(los)
                    case _         =>
                      import ctx.given

                      UserPreferencesQueries.TargetPreferences
                        .queryLineOfSightMotion(props.userId, tid)
                        .runAsyncAndThen: p =>
                          val los = p.toOption.flatten.getOrElse(defaultLOS(props))
                          rvView.set(los)
    } yield
      import ctx.given

      // Create a wrapped view that calls our save function when changed
      val losView = rvView.withOnMod: v =>
        props.preferences
          .zoom(UserPreferences.targetLineOfSightMotion(props.targetId).some)
          .set(v) *>
          UserPreferencesQueries.TargetPreferences
            .upsertLineOfSightMotion(props.userId, props.targetId, v)
            .runAsyncAndForget

      val baseCss = ExploreStyles.Grow(1.refined) |+|
        ExploreStyles.WarningInput.when_(props.rv.get.isEmpty)

      val input = rvView.get match {
        case LineOfSightMotion.Z  =>
          FormInputTextView(
            id = "los-z".refined,
            value = props.rv.zoom(rvToRedshiftGet)(rvToRedshiftMod),
            validFormat =
              InputValidSplitEpi.fromFormat(formatZ, "Must be a number".refined).optional,
            changeAuditor = ChangeAuditor.fromFormat(formatZ).decimal(9.refined).optional,
            groupClass = baseCss,
            disabled = props.disabled,
            postAddons = addons(props.rv.get, props.calibrationRole)
          )
        case LineOfSightMotion.CZ =>
          FormInputTextView(
            id = "los-cz".refined,
            value = props.rv.zoom(rvToARVGet)(rvToARVMod),
            validFormat =
              InputValidSplitEpi.fromFormat(formatCZ, "Must be a number".refined).optional,
            changeAuditor = ChangeAuditor.fromFormat(formatCZ).decimal(10.refined).optional,
            groupClass = baseCss,
            disabled = props.disabled,
            units = "km/s",
            postAddons = addons(props.rv.get, props.calibrationRole)
          )
        case LineOfSightMotion.RV =>
          FormInputTextView(
            id = "los-rv".refined,
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
        FormLabel(htmlFor = "rv-view".refined)(rvView.get.shortName),
        <.div(
          ExploreStyles.FlexContainer |+| ExploreStyles.TargetRVControls |+| LucumaPrimeStyles.FormField,
          EnumDropdownView(id = "rv-view".refined, value = losView, disabled = props.disabled),
          input
        )
      )
}
