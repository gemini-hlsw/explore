// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.effect.IO
import cats.syntax.all._
import crystal.ViewF
import crystal.react.implicits._
import eu.timepit.refined.auto._
import explore.AppCtx
import explore.UnderConstruction
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.model.SpectroscopyConfigurationOptions
import explore.model.enum.ConfigurationMode
import explore.model.enum.FocalPlaneOptions
import explore.model.enum.SpectroscopyCapabilities
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Observation
import lucuma.core.util.Display
import lucuma.ui.forms.EnumViewSelect
import lucuma.ui.reusability._
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
    spectroscopyOptions: SpectroscopyConfigurationOptions
  )

  protected val component =
    ScalaComponent
      .builder[Props]
      .initialState(
        State(ConfigurationMode.Spectroscopy, SpectroscopyConfigurationOptions.Default)
      )
      .render { $ =>
        AppCtx.using { implicit appCtx =>
          val mode           = ViewF.fromState[IO]($).zoom(State.mode)
          val isSpectroscopy = mode.get === ConfigurationMode.Spectroscopy

          val spectroscopy = ViewF.fromState[IO]($).zoom(State.spectroscopyOptions)

          <.div(
            ExploreStyles.ConfigurationGrid,
            Form(size = Small)(
              ExploreStyles.Grid,
              ExploreStyles.Compact,
              ExploreStyles.ConfigurationForm,
              <.label("Mode"),
              EnumViewSelect(id = "configuration-mode", value = mode),
              SpectroscopyConfigurationPanel(spectroscopy).when(isSpectroscopy)
            ),
            UnderConstruction()
          )
        }
      }
      .configure(Reusability.shouldComponentUpdate)
      .build
}
