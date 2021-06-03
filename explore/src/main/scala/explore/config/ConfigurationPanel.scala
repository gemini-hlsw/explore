// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.effect.IO
import cats.syntax.all._
import crystal.ViewF
import crystal.react.implicits._
import crystal.react.reuse._
import eu.timepit.refined.auto._
import explore.AppCtx
import explore.UnderConstruction
import explore.components.HelpIcon
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.components.undo.UndoButtons
import explore.components.undo.UndoRegion
import explore.implicits._
import explore.model.ImagingConfigurationOptions
import explore.model.SpectroscopyConfigurationOptions
import explore.model.enum.ConfigurationMode
import explore.model.enum.FocalPlaneOptions
import explore.model.enum.SpectroscopyCapabilities
import explore.undo.UndoableView
import explore.undo.Undoer
import japgolly.scalajs.react._
import japgolly.scalajs.react.feature.ReactFragment
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Observation
import lucuma.core.util.Display
import lucuma.ui.forms.EnumViewSelect
import lucuma.ui.reusability._
import monocle.Lens
import monocle.macros.Lenses
import react.common._
import react.semanticui.collections.form.Form
import react.semanticui.sizes._

final case class ConfigurationPanel(
  id:            Option[Observation.Id],
  renderInTitle: Tile.RenderInTitle
) extends ReactProps[ConfigurationPanel](ConfigurationPanel.component)

object ConfigurationPanel {
  type Props = ConfigurationPanel

  implicit val modeDisplay: Display[ConfigurationMode]             = Display.by(_.label, _.label)
  implicit val specCapabDisplay: Display[SpectroscopyCapabilities] = Display.by(_.label, _.label)
  implicit val focaLPlaneDisplay: Display[FocalPlaneOptions]       = Display.by(_.label, _.label)
  implicit val propsReuse: Reusability[Props]                      = Reusability.derive
  implicit val stateReuse: Reusability[State]                      = Reusability.never

  @Lenses
  final case class State(
    mode:                ConfigurationMode,
    spectroscopyOptions: SpectroscopyConfigurationOptions,
    imagingOptions:      ImagingConfigurationOptions
  )

  case class UndoView(
    view:   View[State],
    setter: Undoer.Setter[IO, State]
  ) {
    private val undoableView = UndoableView(view, setter)

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
      props:        Props,
      state:        State,
      undoCtx:      Undoer.Context[IO, State]
    )(implicit ctx: AppContextIO): VdomNode = {
      val undoViewSet    =
        UndoView(ViewF.fromState[IO]($), undoCtx.setter)
      def mode           = undoViewSet(State.mode)
      val isSpectroscopy = mode.get === ConfigurationMode.Spectroscopy

      val spectroscopy = undoViewSet(State.spectroscopyOptions)
      val imaging      = undoViewSet(State.imagingOptions)

      ReactFragment(
        props.renderInTitle(<.span(ExploreStyles.TitleStrip)(UndoButtons(state, undoCtx))),
        <.div(
          ExploreStyles.ConfigurationGrid,
          Form(size = Small)(
            ExploreStyles.Grid,
            ExploreStyles.Compact,
            ExploreStyles.ConfigurationForm,
            <.label("Mode", HelpIcon("configuration/mode.md")),
            EnumViewSelect(id = "configuration-mode", value = mode),
            SpectroscopyConfigurationPanel(spectroscopy).when(isSpectroscopy),
            ImagingConfigurationPanel(imaging).unless(isSpectroscopy)
          ),
          UnderConstruction()
        )
      )
    }

    def render(props: Props, state: State) = AppCtx.using { implicit appCtx =>
      UndoRegion[State](Reuse.currying(props, state).in(renderFn _))
    }
  }
  protected val component =
    ScalaComponent
      .builder[Props]
      .initialState(
        State(ConfigurationMode.Imaging,
              SpectroscopyConfigurationOptions.Default,
              ImagingConfigurationOptions.Default
        )
      )
      .renderBackend[Backend]
      .configure(Reusability.shouldComponentUpdate)
      .build
}
