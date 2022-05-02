// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.data.NonEmptyChain
import cats.data.Validated
import cats.syntax.all._
import coulomb.Quantity
import crystal.react._
import crystal.react.hooks._
import crystal.react.implicits._
import crystal.react.reuse._
import eu.timepit.refined.auto._
import explore.common.ObsQueries._
import explore.common.ScienceQueries._
import explore.components.HelpIcon
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.components.undo.UndoButtons
import explore.implicits._
import explore.model.ITCTarget
import explore.model.ImagingConfigurationOptions
import explore.model.PosAngle
import explore.model.SpectroscopyConfigurationOptions
import explore.model.TruncatedPA
import explore.model.display._
import explore.model.enum.PosAngleOptions
import explore.model.formats.angleTruncatedPASplitEpi
import explore.model.reusability._
import explore.model.syntax.all._
import explore.targeteditor.InputWithUnits
import explore.undo.UndoContext
import japgolly.scalajs.react._
import japgolly.scalajs.react.util.syntax._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.enum.ScienceMode
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.core.math.units.Micrometer
import lucuma.core.model.ConstraintSet
import lucuma.core.model.Observation
import lucuma.core.optics.syntax.lens._
import lucuma.core.syntax.string._
import lucuma.ui.forms.EnumViewSelect
import lucuma.ui.optics.ChangeAuditor
import lucuma.ui.optics.ValidFormatInput
import lucuma.ui.reusability._
import monocle.Iso
import monocle.Lens
import react.common._
import react.semanticui.collections.form.Form
import react.semanticui.elements.button.Button
import react.semanticui.sizes._

final case class ConfigurationPanel(
  obsId:            Observation.Id,
  scienceDataUndo:  Reuse[UndoContext[ScienceData]],
  constraints:      ConstraintSet,
  itcTargets:       List[ITCTarget],
  renderInTitle:    Tile.RenderInTitle
)(implicit val ctx: AppContextIO)
    extends ReactFnProps[ConfigurationPanel](ConfigurationPanel.component)

object ConfigurationPanel {
  type Props = ConfigurationPanel

  implicit val propsReuse: Reusability[Props] = Reusability.derive

  val dataIso: Iso[SpectroscopyRequirementsData, SpectroscopyConfigurationOptions] =
    Iso[SpectroscopyRequirementsData, SpectroscopyConfigurationOptions] { s =>
      def wavelengthToMicro(w: Wavelength) = w.micrometer.toValue[BigDecimal]

      val op = for {
        _ <- SpectroscopyConfigurationOptions.wavelengthQ         := s.wavelength.map(wavelengthToMicro)
        _ <- SpectroscopyConfigurationOptions.resolution          := s.resolution
        _ <- SpectroscopyConfigurationOptions.signalToNoise       := s.signalToNoise
        _ <- SpectroscopyConfigurationOptions.signalToNoiseAtQ    := s.signalToNoiseAt.map(
               wavelengthToMicro
             )
        _ <- SpectroscopyConfigurationOptions.wavelengthCoverageQ := s.wavelengthCoverage.map(
               wavelengthToMicro
             )
        _ <- SpectroscopyConfigurationOptions.focalPlane          := s.focalPlane
        _ <- SpectroscopyConfigurationOptions.focalPlaneAngle     := s.focalPlaneAngle
        _ <- SpectroscopyConfigurationOptions.capabilities        := s.capabilities
      } yield ()
      op.runS(SpectroscopyConfigurationOptions.Default).value
    } { s =>
      def microToWavelength(m: Quantity[BigDecimal, Micrometer]) =
        Wavelength.decimalMicrometers.getOption(m.value)

      val op = for {
        _ <- SpectroscopyRequirementsData.wavelength         := s.wavelengthQ.flatMap(microToWavelength)
        _ <- SpectroscopyRequirementsData.resolution         := s.resolution
        _ <- SpectroscopyRequirementsData.signalToNoise      := s.signalToNoise
        _ <- SpectroscopyRequirementsData.signalToNoiseAt    := s.signalToNoiseAtQ.flatMap(
               microToWavelength
             )
        _ <- SpectroscopyRequirementsData.wavelengthCoverage := s.wavelengthCoverageQ.flatMap(
               microToWavelength
             )
        _ <- SpectroscopyRequirementsData.focalPlane         := s.focalPlane
        _ <- SpectroscopyRequirementsData.focalPlaneAngle    := s.focalPlaneAngle
        _ <- SpectroscopyRequirementsData.capabilities       := s.capabilities
      } yield ()
      op.runS(SpectroscopyRequirementsData()).value
    }

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
      .useStateViewWithReuse[ScienceMode](ScienceMode.Spectroscopy)
      .useStateViewWithReuse(PosAngle.Default)
      .useStateViewWithReuse[ImagingConfigurationOptions](ImagingConfigurationOptions.Default)
      .renderWithReuse { (props, mode, posAngle, imaging) =>
        implicit val ctx: AppContextIO = props.ctx
        val requirementsCtx            = props.scienceDataUndo.map(_.zoom(ScienceData.requirements))

        val requirementsViewSet = requirementsCtx.map(UndoView(props.obsId, _))

        val isSpectroscopy = mode.get === ScienceMode.Spectroscopy

        val spectroscopy = requirementsViewSet.map(
          _(
            ScienceRequirementsData.spectroscopy,
            UpdateScienceRequirements.spectroscopyRequirements
          )
        )

        val modeView = props.scienceDataUndo.map(
          _.undoableView(ScienceData.mode)
            .withOnMod(conf => setScienceMode(props.obsId, conf).runAsync)
        )

        val posAngleOptionsView = posAngle.zoom(unsafePosOptionsLens)

        val fixedView = posAngle
          .zoom(PosAngle.fixedAnglePrism)
          .zoom(angleTruncatedPASplitEpi.get)(angleTruncatedPASplitEpi.modify _)

        val allowedFlipView = posAngle
          .zoom(PosAngle.allowFlipAnglePrism)
          .zoom(angleTruncatedPASplitEpi.get)(angleTruncatedPASplitEpi.modify _)

        val parallacticOverrideView = posAngle
          .zoom(PosAngle.parallacticOverrideAnglePrism)
          .zoom(angleTruncatedPASplitEpi.get)(angleTruncatedPASplitEpi.modify _)

        def posAngleEditor(pa: ReuseView[TruncatedPA]) =
          <.div(
            ExploreStyles.SignalToNoiseAt,
            InputWithUnits[ReuseView, TruncatedPA](
              id = "pos-angle-value",
              clazz = Css.Empty,
              value = pa,
              units = "° E of N",
              validFormat = truncatedPAAngle,
              changeAuditor = ChangeAuditor.accept.decimal(2)
            )
          )

        <.div(
          ExploreStyles.ConfigurationGrid,
          props.renderInTitle(
            <.span(ExploreStyles.TitleUndoButtons)(UndoButtons(props.scienceDataUndo))
          ),
          Form(size = Small)(
            ExploreStyles.Compact,
            ExploreStyles.ExploreForm,
            ExploreStyles.ObsConfigurationForm
          )(
            <.label("Position Angle", HelpIcon("configuration/positionangle.md")),
            EnumViewSelect[ReuseView, PosAngleOptions](id = "pos-angle-alternative",
                                                       value = posAngleOptionsView
            ),
            fixedView.mapValue(posAngleEditor),
            allowedFlipView.mapValue(posAngleEditor),
            parallacticOverrideView.mapValue(posAngleEditor)
          ),
          Form(size = Small)(
            ExploreStyles.Compact,
            ExploreStyles.ExploreForm,
            ExploreStyles.ConfigurationForm
          )(
            <.label("Mode", HelpIcon("configuration/mode.md")),
            EnumViewSelect[ReuseView, ScienceMode](id = "configuration-mode", value = mode),
            SpectroscopyConfigurationPanel(spectroscopy.as(dataIso))
              .when(isSpectroscopy),
            ImagingConfigurationPanel(imaging)
              .unless(isSpectroscopy),
            SequenceEditorPopup(
              props.obsId,
              trigger = Reuse.by(props.obsId)(
                Button(
                  size = Small,
                  compact = true,
                  clazz = ExploreStyles.VeryCompact,
                  content = "View Sequence"
                )
              )
            )
          ),
          SpectroscopyModesTable(
            modeView,
            spectroscopy.get,
            props.constraints,
            if (props.itcTargets.isEmpty) none else props.itcTargets.some,
            ctx.staticData.spectroscopyMatrix
          ).when(isSpectroscopy)
        )
      }
}
