// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.syntax.all._
import coulomb.Quantity
import crystal.react.ReuseView
import crystal.react.hooks._
import crystal.react.implicits._
import crystal.react.reuse.Reuse
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
import explore.model.SpectroscopyConfigurationOptions
import explore.model.display._
import explore.model.reusability._
import explore.undo.UndoContext
import japgolly.scalajs.react._
import japgolly.scalajs.react.util.syntax._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.enum.ScienceMode
import lucuma.core.math.Wavelength
import lucuma.core.math.units.Micrometer
import lucuma.core.model.ConstraintSet
import lucuma.core.model.Observation
import lucuma.core.optics.syntax.lens._
import lucuma.ui.forms.EnumViewSelect
import lucuma.ui.reusability._
import monocle.Iso
import react.common._
import react.semanticui.collections.form.Form
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

  protected val component =
    ScalaFnComponent
      .withHooks[Props]
      .useStateViewWithReuse[ScienceMode](ScienceMode.Spectroscopy)
      .useStateViewWithReuse[ImagingConfigurationOptions](ImagingConfigurationOptions.Default)
      .renderWithReuse { (props, mode, imaging) =>
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

        val configurationView = props.scienceDataUndo.map(
          _.undoableView(ScienceData.configuration)
            .withOnMod(conf => setScienceConfiguration(props.obsId, conf).runAsync)
        )

        <.div(
          ExploreStyles.ConfigurationGrid,
          props.renderInTitle(
            <.span(ExploreStyles.TitleUndoButtons)(UndoButtons(props.scienceDataUndo))
          ),
          Form(size = Small)(
            ExploreStyles.Grid,
            ExploreStyles.Compact,
            ExploreStyles.ExploreForm,
            ExploreStyles.ConfigurationForm
          )(
            <.label("Mode", HelpIcon("configuration/mode.md")),
            EnumViewSelect[ReuseView, ScienceMode](id = "configuration-mode", value = mode),
            SpectroscopyConfigurationPanel(spectroscopy.as(dataIso))
              .when(isSpectroscopy),
            ImagingConfigurationPanel(imaging)
              .unless(isSpectroscopy)
          ),
          SpectroscopyModesTable(
            configurationView,
            spectroscopy.get,
            props.constraints,
            if (props.itcTargets.isEmpty) none else props.itcTargets.some,
            ctx.staticData.spectroscopyMatrix
          ).when(isSpectroscopy)
        )
      }
}
