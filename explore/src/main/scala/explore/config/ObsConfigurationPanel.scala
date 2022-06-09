// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.data.NonEmptyChain
import cats.data.Validated
import cats.effect._
import crystal.react._
import crystal.react.implicits._
import eu.timepit.refined.auto._
import explore.common.ObsQueries
import explore.common.ObsQueries._
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.model.TruncatedPA
import explore.model.enum.PosAngleOptions
import explore.model.formats.angleTruncatedPASplitEpi
import explore.model.syntax.all._
import explore.targeteditor.InputWithUnits
import explore.undo._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.math.Angle
import lucuma.core.model.Observation
import lucuma.core.model.PosAngleConstraint
import lucuma.core.syntax.string._
import lucuma.ui.forms.EnumViewSelect
import lucuma.ui.optics.ChangeAuditor
import lucuma.ui.optics.ValidFormatInput
import monocle.Lens
import monocle.std.option
import react.common._
import react.semanticui.collections.form.Form
import react.semanticui.sizes._

final case class ObsConfigurationPanel(
  obsId:            Observation.Id,
  posAngleView:     View[Option[PosAngleConstraint]]
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
  private val unsafePosOptionsLens: Lens[Option[PosAngleConstraint], PosAngleOptions] =
    Lens[Option[PosAngleConstraint], PosAngleOptions](_.toPosAngleOption)((a: PosAngleOptions) =>
      (
        (b: Option[PosAngleConstraint]) =>
          a.toPosAngle(b match {
            case Some(PosAngleConstraint.Fixed(a))               => a
            case Some(PosAngleConstraint.AllowFlip(a))           => a
            case Some(PosAngleConstraint.AverageParallactic)     => Angle.Angle0
            case Some(PosAngleConstraint.ParallacticOverride(a)) => a
            case None                                            => Angle.Angle0
          })
      )
    )

  protected val component =
    ScalaFnComponent[Props] { props =>
      implicit val ctx: AppContextIO = props.ctx

      val paView = props.posAngleView.withOnMod(c =>
        Callback.log(s"$c") *> ObsQueries.updatePosAngle[IO](List(props.obsId), c).runAsync
      )

      val posAngleOptionsView: View[PosAngleOptions] =
        paView.zoom(unsafePosOptionsLens)

      val fixedView: ViewOpt[TruncatedPA] =
        paView
          .zoom(option.some[PosAngleConstraint])
          .zoom(PosAngleConstraint.fixedAngle)
          .zoom(angleTruncatedPASplitEpi.get)(angleTruncatedPASplitEpi.modify _)

      val allowedFlipView: ViewOpt[TruncatedPA] =
        paView
          .zoom(option.some[PosAngleConstraint])
          .zoom(PosAngleConstraint.allowFlipAngle)
          .zoom(angleTruncatedPASplitEpi.get)(angleTruncatedPASplitEpi.modify _)

      val parallacticOverrideView: ViewOpt[TruncatedPA] =
        paView
          .zoom(option.some[PosAngleConstraint])
          .zoom(PosAngleConstraint.parallacticOverrideAngle)
          .zoom(angleTruncatedPASplitEpi.get)(angleTruncatedPASplitEpi.modify _)

      def posAngleEditor(pa: View[TruncatedPA]) =
        <.div(
          ExploreStyles.InputWithLabel,
          InputWithUnits(
            id = "pos-angle-value",
            clazz = Css.Empty,
            value = pa,
            units = "° E of N",
            validFormat = truncatedPAAngle,
            changeAuditor = ChangeAuditor.accept.decimal(2)
          )
        )

      Form(size = Small)(
        ExploreStyles.Compact,
        ExploreStyles.ObsConfigurationForm
      )(
        <.label("Position Angle", HelpIcon("configuration/positionangle.md")),
        EnumViewSelect(
          clazz = ExploreStyles.ObsConfigurationObsPA,
          id = "pos-angle-alternative",
          value = posAngleOptionsView
        ),
        fixedView.mapValue(posAngleEditor),
        allowedFlipView.mapValue(posAngleEditor),
        parallacticOverrideView.mapValue(posAngleEditor)
      )
    }

}
