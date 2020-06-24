// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.effect.IO
import cats.implicits._
import crystal.ViewF
import crystal.react.implicits._
import explore.implicits._
import explore.model.SiderealTarget
import explore.undo.Undoer
import gpp.ui.forms._
import gsp.math.Coordinates
import gsp.math.Declination
import gsp.math.RightAscension
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.html_<^._
import monocle.macros.Lenses
import react.common._
import react.semanticui.collections.form._
import react.semanticui.elements.button.Button
import react.semanticui.elements.icon.Icon
import react.semanticui.modules.dropdown.DropdownItem
import react.semanticui.sizes._
import react.semanticui.widths._
import gsp.math.ProperMotion
import monocle.Lens
import explore.data.Optics
import monocle.Iso
import cats.effect.Async
import cats.effect.ContextShift
import explore.AppCtx

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

  private val coordsLens: Lens[SiderealTarget, Coordinates] =
    SiderealTarget.track.composeLens(ProperMotion.baseCoordinates)

  private val raLens: Lens[SiderealTarget, RightAscension] =
    coordsLens.composeLens(Coordinates.rightAscension)

  private val decLens: Lens[SiderealTarget, Declination] =
    coordsLens.composeLens(Coordinates.declination)

  private val stateLens: Lens[SiderealTarget, State] =
    Optics
      .disjointZip(SiderealTarget.name, raLens, decLens)
      .composeIso(Iso((State.apply _).tupled) { case State(name, ra, dec) => (name, ra, dec) })

  // BEGIN Move to crystal
  implicit class ViewFModuleOps(val viewFModule: ViewF.type) extends AnyVal {
    def fromState[F[_]]: FromStateApply[F] =
      new FromStateApply[F]()
  }

  class FromStateApply[F[_]]() {
    def apply[S](
      $              : BackendScope[_, S]
    )(implicit async: Async[F], cs: ContextShift[F]): ViewF[F, S] =
      ViewF($.state.runNow(), $.modStateIn[F])
  }
  // END Move to crystal

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
          FormInputView(name = "search",
                        id = "search",
                        view = stateView.zoom(State.searchTerm),
                        label = "Target",
                        focus = true,
                        icon = Icon("search")
          ),
          FormGroup(widths = FormWidths.Equal)(
            FormInputView(
              width = Seven,
              name = "ra",
              id = "ra",
              view = stateView.zoom(State.raValue),
              optic = InputOptics.fromFormat(RightAscension.fromStringHMS),
              label = "RA"
            ),
            FormInputView(
              width = Seven,
              name = "dec",
              id = "dec",
              view = stateView.zoom(State.decValue),
              optic = InputOptics.fromFormat(Declination.fromStringSignedDMS),
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
