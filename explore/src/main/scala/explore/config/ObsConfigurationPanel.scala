// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.effect.*
import crystal.react.*
import crystal.react.implicits.*
import eu.timepit.refined.auto.*
import explore.components.HelpIcon
import explore.components.InputWithUnits
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.enums.PosAngleOptions
import explore.model.syntax.all.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.math.Angle
import lucuma.core.math.validation.MathValidators
import lucuma.core.model.Observation
import lucuma.core.model.PosAngleConstraint
import lucuma.refined.*
import lucuma.ui.forms.EnumViewSelect
import lucuma.ui.input.ChangeAuditor
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import monocle.Lens
import monocle.std.option
import queries.schemas.odb.ObsQueries
import react.common.Css
import react.common.ReactFnProps
import react.semanticui.collections.form.Form
import react.semanticui.sizes.*
import explore.model.enums.AgsState

case class ObsConfigurationPanel(
  obsId:        Observation.Id,
  posAngleView: View[Option[PosAngleConstraint]],
  agsState:     AgsState
) extends ReactFnProps(ObsConfigurationPanel.component)

object ObsConfigurationPanel:
  private type Props = ObsConfigurationPanel

  /**
   * Used to convert pos angle and an enumeration for a UI selector It is unsafe as the angle is
   * lost for Average Parallictic and Unconstrained
   */
  private val unsafePosOptionsLens: Lens[Option[PosAngleConstraint], PosAngleOptions] =
    Lens[Option[PosAngleConstraint], PosAngleOptions](_.toPosAngleOptions)((a: PosAngleOptions) =>
      (b: Option[PosAngleConstraint]) =>
        a.toPosAngle(b match {
          case Some(PosAngleConstraint.Fixed(a))               => a
          case Some(PosAngleConstraint.AllowFlip(a))           => a
          case Some(PosAngleConstraint.AverageParallactic)     => Angle.Angle0
          case Some(PosAngleConstraint.ParallacticOverride(a)) => a
          case None                                            => Angle.Angle0
        })
    )

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .render { (props, ctx) =>
        import ctx.given
        println(props.agsState)

        val paView = props.posAngleView
          .withOnMod(c => ObsQueries.updatePosAngle[IO](List(props.obsId), c).runAsync)

        val posAngleOptionsView: View[PosAngleOptions] =
          paView.zoom(unsafePosOptionsLens)

        val fixedView: ViewOpt[Angle] =
          paView
            .zoom(option.some[PosAngleConstraint])
            .zoom(PosAngleConstraint.fixedAngle)

        val allowedFlipView: ViewOpt[Angle] =
          paView
            .zoom(option.some[PosAngleConstraint])
            .zoom(PosAngleConstraint.allowFlipAngle)

        val parallacticOverrideView: ViewOpt[Angle] =
          paView
            .zoom(option.some[PosAngleConstraint])
            .zoom(PosAngleConstraint.parallacticOverrideAngle)

        def posAngleEditor(pa: View[Angle]) =
          <.div(
            ExploreStyles.InputWithLabel,
            InputWithUnits(
              id = "pos-angle-value".refined,
              clazz = Css.Empty,
              value = pa,
              units = "° E of N",
              disabled = !props.agsState.canRecalculate,
              validFormat = MathValidators.truncatedAngleDegrees,
              changeAuditor = ChangeAuditor.bigDecimal(3.refined, 2.refined)
            )
          )

        Form(size = Small)(
          ExploreStyles.Compact,
          ExploreStyles.ObsConfigurationForm
        )(
          <.label("Position Angle", HelpIcon("configuration/positionangle.md".refined)),
          EnumViewSelect(
            clazz = ExploreStyles.ObsConfigurationObsPA,
            id = "pos-angle-alternative",
            value = posAngleOptionsView,
            disabled = !props.agsState.canRecalculate
          ),
          fixedView.mapValue(posAngleEditor),
          allowedFlipView.mapValue(posAngleEditor),
          parallacticOverrideView.mapValue(posAngleEditor)
        )
      }
