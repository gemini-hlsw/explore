// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.data.NonEmptyChain
import cats.data.Validated
import crystal.react._
import eu.timepit.refined.auto._
// import explore.common.ObsEditQueries._
import explore.common.ObsQueries._
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.model.ObsConfiguration
import explore.model.TruncatedPA
import explore.model.enum.PosAngleOptions
import explore.model.formats.angleTruncatedPASplitEpi
import explore.model.syntax.all._
import explore.targeteditor.InputWithUnits
import explore.undo._
import japgolly.scalajs.react._
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.math.Angle
import lucuma.core.model.Observation
import lucuma.core.model.PosAngle
import lucuma.core.syntax.string._
import lucuma.ui.forms.EnumViewSelect
import lucuma.ui.optics.ChangeAuditor
import lucuma.ui.optics.ValidFormatInput
import monocle.Lens
import react.common._
import react.semanticui.collections.form.Form
import react.semanticui.sizes._

final case class ObsConfigurationPanel(
  obsId:            Observation.Id,
  obsCtx1:          UndoSetter[ObservationData],
  obsConf:          View[ObsConfiguration]
)(implicit val ctx: AppContextIO)
    extends ReactFnProps[ObsConfigurationPanel](ObsConfigurationPanel.component)

object ObsConfigurationPanel {
  type Props = ObsConfigurationPanel

  // Input for an angle in degrees with up to 2 decimals
  private val truncatedPAAngle = ValidFormatInput[TruncatedPA](
    s => {
      val ota = s.parseDoubleOption
        .map(Angle.fromDoubleDegrees)
        .map(TruncatedPA(_))
      Validated.fromOption(ota, NonEmptyChain("Invalid Position Angle"))
    },
    pa => f"${pa.angle.toDoubleDegrees}%.2f"
  )

  /**
   * Used to convert pos angle and an enumeration for a UI selector It is unsafe as the angle is
   * lost for Average Parallictic and Unconstrained
   */
  private val unsafePosOptionsLens: Lens[PosAngle, PosAngleOptions] =
    Lens[PosAngle, PosAngleOptions](_.toPosAngleOption)((a: PosAngleOptions) =>
      (
        (b: PosAngle) =>
          a.toPosAngle(b match {
            case PosAngle.Fixed(a)               => a
            case PosAngle.AllowFlip(a)           => a
            case PosAngle.AverageParallactic     => Angle.Angle0
            case PosAngle.ParallacticOverride(a) => a
            case PosAngle.Unconstrained          => Angle.Angle0
          })
      )
    )

  protected val component =
    ScalaFnComponent
      .withHooks[Props]
      .render { props =>
        // implicit val ctx: AppContextIO = props.ctx

        // Will be used for posAngle
        // val obsUndoView: ObsUndoView = ObsUndoView(props.obsId, props.obsCtx)

        val posAngleOptionsView: View[PosAngleOptions] =
          props.obsConf.zoom(ObsConfiguration.posAngle.andThen(unsafePosOptionsLens))

        val fixedView: ViewOpt[TruncatedPA] =
          props.obsConf
            .zoom(ObsConfiguration.posAngle)
            .zoom(PosAngle.fixedAnglePrism)
            .zoom(angleTruncatedPASplitEpi.get)(angleTruncatedPASplitEpi.modify _)

        val allowedFlipView: ViewOpt[TruncatedPA] =
          props.obsConf
            .zoom(ObsConfiguration.posAngle)
            .zoom(PosAngle.allowFlipAnglePrism)
            .zoom(angleTruncatedPASplitEpi.get)(angleTruncatedPASplitEpi.modify _)

        val parallacticOverrideView: ViewOpt[TruncatedPA] =
          props.obsConf
            .zoom(ObsConfiguration.posAngle)
            .zoom(PosAngle.parallacticOverrideAnglePrism)
            .zoom(angleTruncatedPASplitEpi.get)(angleTruncatedPASplitEpi.modify _)

        def posAngleEditor(pa: View[TruncatedPA]) =
          <.div(
            ExploreStyles.SignalToNoiseAt,
            InputWithUnits(
              id = "pos-angle-value",
              clazz = Css.Empty,
              value = pa,
              units = "° E of N",
              validFormat = truncatedPAAngle,
              changeAuditor = ChangeAuditor.accept.decimal(2)
            )
          )

        ReactFragment(
          Form(size = Small)(
            ExploreStyles.Compact,
            ExploreStyles.ObsConfigurationForm
          )(
            <.div(
              ExploreStyles.ObsConfigurationObsPA,
              <.label("Position Angle", HelpIcon("configuration/positionangle.md")),
              EnumViewSelect(
                id = "pos-angle-alternative",
                value = posAngleOptionsView
              ),
              fixedView.mapValue(posAngleEditor),
              allowedFlipView.mapValue(posAngleEditor),
              parallacticOverrideView.mapValue(posAngleEditor)
            )
          )
        )
      }

}
