// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.target

import cats.implicits._
import explore._
import explore.implicits._
import crystal.react.implicits._
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
import react.semanticui.elements.icon.Icon
import react.semanticui.modules.dropdown.DropdownItem
import react.semanticui.sizes._
import react.semanticui.widths._
import explore.model.SiderealTarget
import explore.undo.Undoer
import cats.effect.IO

final case class CoordinatesForm(
  target:           SiderealTarget,
  searchAndGo:      String => Callback,
  goToRaDec:        Coordinates => Callback,
  undoCtx:          Undoer.Context[IO, SiderealTarget]
)(implicit val ctx: AppContextIO)
    extends ReactProps {
  @inline override def render: VdomElement = CoordinatesForm.component(this)
}

object CoordinatesForm {
  type Props = CoordinatesForm

  @Lenses
  final case class State(
    initialSearchTerm: String,
    searchTerm:        String,
    raValue:           RightAscension,
    decValue:          Declination
  ) {
    val aladinCoords: String  = Coordinates.fromHmsDms.reverseGet(Coordinates(raValue, decValue))
    val shouldSearch: Boolean = initialSearchTerm =!= searchTerm
  }

  class Backend(bs: BackendScope[Props, State]) {

    def updateSearchOp(value: Option[String], cb: Callback = Callback.empty): Callback =
      bs.setStateOptionL(State.searchTerm)(value, cb)

    def setRa(ra: Option[RightAscension], cb: Callback = Callback.empty): Callback =
      bs.setStateOptionL(State.raValue)(ra, cb)

    def setDec(dec: Option[Declination], cb: Callback = Callback.empty): Callback =
      bs.setStateOptionL(State.decValue)(dec, cb)

    def render(props:       Props, state: State) = {
      val raEV     =
        StateSnapshot[RightAscension](state.raValue)(setRa)
      val decEV    =
        StateSnapshot[Declination](state.decValue)(setDec)
      val searchEV =
        StateSnapshot[String](state.searchTerm)(updateSearchOp)

      Form(size = Mini, onSubmit = props.searchAndGo(searchEV.value).when(state.shouldSearch).void)(
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
                    snapshot = searchEV,
                    label = "Target",
                    focus = true,
                    icon = Icon("search")
        ),
        FormGroup(widths = FormWidths.Equal)(
          FormInputEV(
            width = Seven,
            name = "ra",
            id = "ra",
            snapshot = raEV,
            optic = InputOptics.fromFormat(RightAscension.fromStringHMS),
            label = "RA"
          ),
          FormInputEV(width = Seven,
                      name = "dec",
                      id = "dec",
                      snapshot = decEV,
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
        ),
        FormButton(onClick = props.undoCtx.undo(props.target).runInCB,
                   disabled = props.undoCtx.undoEmpty
        )(
          "Undo"
        )
      )
    }

    def newProps(nextProps: Props): Callback =
      bs.setState(stateFromProps(nextProps))
  }

  def stateFromProps(props: Props): State = {
    val target = props.target
    val coords = target.track.baseCoordinates
    State(target.name, target.name, coords.ra, coords.dec)
  }

  val component =
    ScalaComponent
      .builder[Props]("CoordinatesForm")
      .initialStateFromProps(stateFromProps)
      .renderBackend[Backend]
      .componentWillReceiveProps($ => $.setState(stateFromProps($.nextProps)))
      .build

}
