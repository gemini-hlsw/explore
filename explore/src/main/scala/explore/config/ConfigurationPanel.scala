// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.syntax.all._
import coulomb.Quantity
import crystal.ViewF
import crystal.react.implicits._
import eu.timepit.refined.auto._
import explore.AppCtx
import explore.common.ObsQueries._
import explore.common.ScienceQueries._
import explore.components.HelpIcon
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.components.undo.UndoButtons
import explore.implicits._
import explore.model.AvailableFilter
import explore.model.ImagingConfigurationOptions
import explore.model.SpectroscopyConfigurationOptions
import explore.model.display._
import explore.model.reusability._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.enum.ScienceMode
import lucuma.core.math.Wavelength
import lucuma.core.math.units.Micrometer
import lucuma.core.model.Observation
import lucuma.core.optics.syntax.lens._
import lucuma.ui.forms.EnumViewSelect
import lucuma.ui.reusability._
import monocle.Focus
import monocle.Iso
import monocle.Lens
import react.common._
import react.semanticui.collections.form.Form
import react.semanticui.sizes._

import scala.collection.SortedSet

final case class ConfigurationPanel(
  obsId:            Observation.Id,
  scienceDataUndo:  UndoCtx[ScienceData],
  renderInTitle:    Tile.RenderInTitle
)(implicit val ctx: AppContextIO)
    extends ReactProps[ConfigurationPanel](ConfigurationPanel.component)

object ConfigurationPanel {
  type Props = ConfigurationPanel

  implicit val propsReuse: Reusability[Props]                          = Reusability.derive
  implicit val filterReuse: Reusability[AvailableFilter]               = Reusability.byEq
  implicit val filterSetReuse: Reusability[SortedSet[AvailableFilter]] = Reusability.by(_.toSet)
  implicit val optionsReuse: Reusability[ImagingConfigurationOptions]  = Reusability.derive
  implicit val stateReuse: Reusability[State]                          = Reusability.derive

  final case class State(
    mode:           ScienceMode,
    imagingOptions: ImagingConfigurationOptions
  )

  object State {
    val mode: Lens[State, ScienceMode]                           = Focus[State](_.mode)
    val imagingOptions: Lens[State, ImagingConfigurationOptions] = Focus[State](_.imagingOptions)
  }

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

  class Backend($ : BackendScope[Props, State]) {
    private def renderFn(
      props:           Props,
      scienceDataUndo: UndoCtx[ScienceData]
    )(implicit ctx:    AppContextIO): VdomNode = {
      val requirementsCtx = scienceDataUndo.zoom(ScienceData.requirements)

      val requirementsViewSet = UndoView(props.obsId, requirementsCtx)

      def mode           = requirementsViewSet(ScienceRequirementsData.mode, UpdateScienceRequirements.mode)
      val isSpectroscopy = mode.get === ScienceMode.Spectroscopy

      val spectroscopy = requirementsViewSet(
        ScienceRequirementsData.spectroscopyRequirements,
        UpdateScienceRequirements.spectroscopyRequirements
      )

      val imaging = ViewF.fromState($).zoom(State.imagingOptions)

      val configurationView = scienceDataUndo
        .undoableView(ScienceData.configuration)
        .withOnMod(conf => setScienceConfiguration(props.obsId, conf).runAsync)

      <.div(
        ExploreStyles.ConfigurationGrid,
        props.renderInTitle(<.span(ExploreStyles.TitleUndoButtons)(UndoButtons(scienceDataUndo))),
        Form(size = Small)(
          ExploreStyles.Grid,
          ExploreStyles.Compact,
          ExploreStyles.ExploreForm,
          ExploreStyles.ConfigurationForm
        )(
          <.label("Mode", HelpIcon("configuration/mode.md")),
          EnumViewSelect(id = "configuration-mode", value = mode),
          SpectroscopyConfigurationPanel(spectroscopy.as(dataIso))
            .when(isSpectroscopy),
          ImagingConfigurationPanel(imaging)
            .unless(isSpectroscopy)
        ),
        SpectroscopyModesTable
          .component(
            SpectroscopyModesTable(
              configurationView,
              props.ctx.staticData.spectroscopyMatrix,
              spectroscopy.get
            )
          )
          .when(isSpectroscopy)
      )
    }

    def render(props: Props) = AppCtx.using { implicit appCtx =>
      renderFn(
        props,
        props.scienceDataUndo
      )
    }
  }

  protected val component =
    ScalaComponent
      .builder[Props]
      .initialState(State(ScienceMode.Spectroscopy, ImagingConfigurationOptions.Default))
      .renderBackend[Backend]
      .configure(Reusability.shouldComponentUpdate)
      .build
}
