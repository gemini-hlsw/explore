// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.effect.IO
import cats.effect.SyncIO
import cats.syntax.all._
import coulomb.refined._
import crystal.Pot
import crystal.ViewF
import crystal.react.implicits._
import eu.timepit.refined.auto._
import eu.timepit.refined.numeric.Positive
import explore.AppCtx
import explore.components.HelpIcon
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.components.undo.UndoButtons
import explore.implicits._
import explore.model.ImagingConfigurationOptions
import explore.model.SpectroscopyConfigurationOptions
import explore.model.enum.ConfigurationMode
import explore.model.enum.FocalPlane
import explore.model.enum.SpectroscopyCapabilities
import explore.modes.SpectroscopyModesMatrix
import explore.undo.UndoContext
import explore.undo.UndoStacks
import explore.undo.UndoableView
import fs2._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Observation
import lucuma.core.util.Display
import lucuma.ui.forms.EnumViewSelect
import lucuma.ui.reusability._
import monocle.Lens
import monocle.Focus
import react.common._
import react.semanticui.collections.form.Form
import react.semanticui.sizes._
import sttp.client3._
import sttp.client3.impl.cats.FetchCatsBackend
import sttp.model._

import scala.concurrent.duration._

final case class ConfigurationPanel(
  id:               Option[Observation.Id],
  renderInTitle:    Tile.RenderInTitle
)(implicit val ctx: AppContextIO)
    extends ReactProps[ConfigurationPanel](ConfigurationPanel.component)

object ConfigurationPanel {
  type Props = ConfigurationPanel

  implicit val modeDisplay: Display[ConfigurationMode]             = Display.by(_.label, _.label)
  implicit val specCapabDisplay: Display[SpectroscopyCapabilities] = Display.by(_.label, _.label)
  implicit val focaLPlaneDisplay: Display[FocalPlane]              = Display.by(_.label, _.label)
  implicit val propsReuse: Reusability[Props]                      = Reusability.derive
  implicit val stateReuse: Reusability[State]                      = Reusability.never

  final case class State(
    mode:                ConfigurationMode,
    spectroscopyOptions: SpectroscopyConfigurationOptions,
    imagingOptions:      ImagingConfigurationOptions,
    matrix:              Pot[SpectroscopyModesMatrix]
  )

  object State {
    val mode                = Focus[State](_.mode)
    val spectroscopyOptions = Focus[State](_.spectroscopyOptions)
    val imagingOptions      = Focus[State](_.imagingOptions)
    val matrix              = Focus[State](_.matrix)
  }

  case class UndoView(
    undoCtx: UndoCtx[State]
  ) {
    private val undoableView = UndoableView(undoCtx)

    def apply[A](
      modelGet: State => A,
      modelMod: (A => A) => State => State
    ): View[A] =
      undoableView.apply(
        modelGet,
        modelMod,
        _ => IO.unit
      )

    def apply[A](
      lens: Lens[State, A]
    ): View[A] =
      apply(lens.get, lens.modify)

  }

  class Backend($ : BackendScope[Props, State]) {
    private def renderFn(
      props:   Props,
      undoCtx: UndoCtx[State]
    ): VdomNode = {
      val undoViewSet = UndoView(undoCtx)

      def mode           = undoViewSet(State.mode)
      val isSpectroscopy = mode.get === ConfigurationMode.Spectroscopy

      val spectroscopy = undoViewSet(State.spectroscopyOptions)
      val imaging      = undoViewSet(State.imagingOptions)

      <.div(
        ExploreStyles.ConfigurationGrid,
        props.renderInTitle(<.span(ExploreStyles.TitleStrip)(UndoButtons(undoCtx))),
        Form(size = Small)(
          ExploreStyles.Grid,
          ExploreStyles.Compact,
          ExploreStyles.ExploreForm,
          ExploreStyles.ConfigurationForm,
          <.label("Mode", HelpIcon("configuration/mode.md")),
          EnumViewSelect(id = "configuration-mode", value = mode),
          SpectroscopyConfigurationPanel(spectroscopy).when(isSpectroscopy),
          ImagingConfigurationPanel(imaging).unless(isSpectroscopy)
        ),
        SpectroscopyModesTable(
          undoCtx.model.get.matrix.toOption
            .map(
              _.filtered(
                focalPlane = spectroscopy.get.focalPlane,
                capabilities = spectroscopy.get.capabilities,
                wavelength = spectroscopy.get.wavelength,
                slitWidth = spectroscopy.get.focalPlaneAngle,
                resolution = spectroscopy.get.resolution,
                range = spectroscopy.get.wavelengthRange
                  .map(_.micrometer.toValue[BigDecimal].toRefined[Positive])
              )
            )
            .getOrElse(Nil),
          spectroscopy.get.focalPlane,
          spectroscopy.get.wavelength
        ).when(isSpectroscopy)
      )
    }

    def render(props: Props) = AppCtx.using { implicit appCtx =>
      renderFn(
        props,
        UndoContext(ViewF[SyncIO, UndoStacks[IO, State]](UndoStacks.empty, (_, _) => SyncIO.unit),
                    ViewF.fromStateSyncIO($)
        )
      )
    }
  }

  def load(uri: Uri): IO[SpectroscopyModesMatrix] = {
    val backend = FetchCatsBackend[IO]()
    basicRequest
      .get(uri)
      .readTimeout(5.seconds)
      .send(backend)
      .flatMap {
        _.body.fold(_ => SpectroscopyModesMatrix.empty.pure[IO],
                    s => SpectroscopyModesMatrix[IO](Stream.emit(s))
        )
      }
  }

  protected val component =
    ScalaComponent
      .builder[Props]
      .initialState(
        State(ConfigurationMode.Spectroscopy,
              SpectroscopyConfigurationOptions.Default,
              ImagingConfigurationOptions.Default,
              Pot.pending
        )
      )
      .renderBackend[Backend]
      .componentDidMount { $ =>
        implicit val ctx = $.props.ctx
        load(uri"/instrument_spectroscopy_matrix.csv").flatMap { m =>
          $.modStateIn[IO](State.matrix.replace(Pot(m)))
        }.runAsyncAndForgetCB
      }
      .configure(Reusability.shouldComponentUpdate)
      .build
}
