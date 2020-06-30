// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.effect.Async
import cats.effect.ContextShift
import cats.effect.IO
import cats.implicits._
import crystal.ViewF
import crystal.react.implicits._
import explore.AppCtx
import explore.data.Optics
import explore.implicits._
import explore.model.ModelOptics._
import explore.model.SiderealTarget
import explore.undo.Undoer
import gpp.ui.forms._
import gsp.math.Coordinates
import gsp.math.Declination
import gsp.math.ProperMotion
import gsp.math.RightAscension
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.html_<^._
import monocle.Iso
import monocle.Lens
import monocle.macros.Lenses
import react.common._
import react.semanticui.collections.form._
import react.semanticui.elements.button.Button
import react.semanticui.elements.icon.Icon
import react.semanticui.modules.dropdown.DropdownItem
import react.semanticui.sizes._
import react.semanticui.widths._

final case class CoordinatesForm(
  target:           SiderealTarget,
  searchAndGo:      String => Callback,
  goToRaDec:        Coordinates => Callback
)(implicit val ctx: AppContextIO)
    extends ReactProps[CoordinatesForm](CoordinatesForm.component)

object CoordinatesForm {
  type Props = CoordinatesForm

  @Lenses
  final case class State(
    searchTerm: String,
    raValue:    RightAscension,
    decValue:   Declination
  )

  private val stateLens: Lens[SiderealTarget, State] =
    targetPropsL
      .composeIso(Iso((State.apply _).tupled) { case State(name, ra, dec) => (name, ra, dec) })

  class Backend($ : BackendScope[Props, State]) {

    def render(props: Props, state: State) =
      AppCtx.withCtx { implicit appCtx =>
        val stateView = ViewF.fromState[IO]($)

        Form(
          size = Mini,
          onSubmit = props
            .searchAndGo(state.searchTerm)
            .when(state.searchTerm =!= props.target.name)
            .void
        )(
          FormDropdown(
            label = "Type",
            value = 0,
            selection = true,
            options = List(DropdownItem(value = 0, text = "Sidereal"),
                           DropdownItem(value = 1, text = "Non-sidereal")
            )
          ),
          FormInputEV(name = "search",
                      id = "search",
                      value = stateView.zoom(State.searchTerm),
                      label = "Target",
                      focus = true,
                      icon = Icon("search")
          ),
          FormGroup(widths = FormWidths.Equal)(
            FormInputEV(
              width = Seven,
              name = "ra",
              id = "ra",
              value = stateView.zoom(State.raValue),
              format = RightAscension.fromStringHMS,
              label = "RA"
            ),
            FormInputEV(
              width = Seven,
              name = "dec",
              id = "dec",
              value = stateView.zoom(State.decValue),
              format = Declination.fromStringSignedDMS,
              label = "Dec"
            ),
            FormButton(width = Two,
                       size = Small,
                       icon = true,
                       label = "X",
                       onClick = props.goToRaDec(Coordinates(state.raValue, state.decValue))
            )(
              Icon("angle right")
            )
          )
        )
      }
  }

  val component =
    ScalaComponent
      .builder[Props]
      .initialStateFromProps((stateLens.get _).compose(_.target))
      .renderBackend[Backend]
      .build

}
