// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.target

import cats.effect.IO
import cats.implicits._
import crystal.View
import crystal.react.implicits._
import explore._
import explore.components.ui.GPPStyles
import explore.components.undo.UndoRegion
import explore.implicits._
import gem.EphemerisKey
import gem.Observation
import gem.Target
import gpp.ui.forms._
import gsp.math.Angle
import gsp.math.Coordinates
import gsp.math.Declination
import gsp.math.HourAngle
import gsp.math.ProperMotion
import gsp.math.RightAscension
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.html_<^._
import monocle.Lens
import monocle.macros.Lenses
import react.aladin.Aladin
import react.common._
import react.semanticui.collections.form._
import react.semanticui.collections.grid._
import react.semanticui.elements.icon.Icon
import react.semanticui.modules.dropdown.DropdownItem
import react.semanticui.sizes._
import react.semanticui.widths._

final case class TargetBody(
  observationId:    Observation.Id,
  target:           View[IO, Target]
)(implicit val ctx: AppContextIO)
    extends ReactProps {
  @inline override def render: VdomElement = TargetBody.component(this)
  val aladinCoords: Option[Coordinates]    = target.get.track match {
    case Right(pm) => pm.baseCoordinates.some
    case _         => none
  }
}

object TargetBody      {
  type Props = TargetBody
  val AladinComp = Aladin.component

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
    // Create a mutable reference
    private val ref = Ref.toScalaComponent(AladinComp)

    def updateSearchOp(value: Option[String], cb: Callback = Callback.empty): Callback =
      bs.setStateOptionL(State.searchTerm)(value, cb)

    def setRa(ra: Option[RightAscension], cb: Callback = Callback.empty): Callback =
      bs.setStateOptionL(State.raValue)(ra, cb)

    def setDec(dec: Option[Declination], cb: Callback = Callback.empty): Callback =
      bs.setStateOptionL(State.decValue)(dec, cb)

    val pmRa: Lens[ProperMotion, RightAscension]       =
      ProperMotion.baseCoordinates ^|-> Coordinates.rightAscension
    val pmDec: Lens[ProperMotion, Declination]         =
      ProperMotion.baseCoordinates ^|-> Coordinates.declination
    val targetRA: Lens[Target, Option[RightAscension]] =
      Target.track ^|-> Lens[Either[EphemerisKey, ProperMotion], Option[RightAscension]](
        _.toOption.map(pmRa.get)
      )(ra => t => t.bimap(identity, pmRa.set(ra.getOrElse(RightAscension.Zero))))
    val targetDec: Lens[Target, Option[Declination]]   =
      Target.track ^|-> Lens[Either[EphemerisKey, ProperMotion], Option[Declination]](
        _.toOption.map(pmDec.get)
      )(dec => t => t.bimap(identity, pmDec.set(dec.getOrElse(Declination.Zero))))
    val targetPropsL                                   =
      Lens[Target, (String, Option[RightAscension], Option[Declination])](t =>
        (t.name, targetRA.get(t), targetDec.get(t))
      )(s => t => targetRA.set(s._2)(targetDec.set(s._3)(t.copy(name = s._1))))

    def render(props:          Props, state:     State)           = {
      implicit val appCtx = props.ctx
      val raEV            =
        StateSnapshot[RightAscension](state.raValue)(setRa)
      val decEV           =
        StateSnapshot[Declination](state.decValue)(setDec)
      val searchEV        =
        StateSnapshot[String](state.searchTerm)(updateSearchOp)

      val target = props.target.get
      UndoRegion[Target] { undoCtx =>
        val modifyIO =
          TargetEditor.Modify(props.observationId, target, props.target.set, undoCtx.setter)
        def modify[A](
          lens:   Lens[Target, A],
          fields: A => TargetEditor.Mutation.Fields
        ): A => Callback = { v: A =>
          modifyIO(lens, fields)(v).runInCB
        }
        def goTo(search: String): Callback =
          ref.get
            .flatMapCB(
              _.backend
                .gotoObject(
                  search,
                  (a, b) => {
                    val ra  = RightAscension.fromHourAngle.get(
                      HourAngle.angle.reverseGet(Angle.fromDoubleDegrees(a.toDouble))
                    )
                    val dec =
                      Declination.fromAngle
                        .getOption(Angle.fromDoubleDegrees(b.toDouble))
                        .getOrElse(Declination.Zero)
                    setRa(ra.some) *> setDec(dec.some) *> modify[
                      (String, Option[RightAscension], Option[Declination])
                    ](
                      targetPropsL,
                      {
                        case (n, r, d) =>
                          TargetEditor.Mutation.Fields(
                            name = n.some,
                            ra = r.map(RightAscension.fromStringHMS.reverseGet),
                            dec = d.map(Declination.fromStringSignedDMS.reverseGet)
                          )
                      }
                    )((search, ra.some, dec.some))
                  },
                  Callback.log("error")
                )
            )

        <.div(
          ^.height := "100%",
          ^.width := "100%",
          ^.cls := "check",
          Grid(columns = Two, stretched = true, padded = GridPadded.Horizontally)(
            ^.height := "100%",
            GridRow(stretched = true)(
              GridColumn(stretched = true, computer = Four, clazz = GPPStyles.GPPForm)(
                Form(size = Mini, onSubmit = goTo(searchEV.value).when(state.shouldSearch).void)(
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
                    FormButton(width = Two, size = Small, icon = true, label = "X")(
                      Icon("angle right")
                    )
                  )
                )
              ),
              GridColumn(stretched = true, computer = Twelve)(
                AladinComp.withRef(ref) {
                  Aladin(target = state.aladinCoords, fov = 0.25, showGotoControl = false)
                }
              )
            )
          )
        )
      }
    }

    def newProps(currentProps: Props, nextProps: Props): Callback =
      bs.setState(stateFromProps(nextProps)) *> Callback.log(currentProps.toString()) *>
        nextProps.aladinCoords
          .map(c =>
            Callback.log(s"${c.ra.toHourAngle.toDoubleHours}, ${c.dec.toAngle.toDoubleDegrees}")
          )
          .getOrEmpty *>
        ref.get
          .flatMap(r =>
            CallbackOption.liftOptionCallback(
              nextProps.aladinCoords.map(c =>
                r.backend.gotoRaDec(c.ra.toAngle.toDoubleDegrees, c.dec.toAngle.toDoubleDegrees)
              )
            )
          )
          .when(nextProps.aladinCoords =!= currentProps.aladinCoords)

  }

  def stateFromProps(props: Props): State = {
    val target = props.target.get
    val coords = target.track match {
      case Right(pm) => pm.baseCoordinates
      case _         => Coordinates.Zero
    }
    State(target.name, target.name, coords.ra, coords.dec)
  }

  val component =
    ScalaComponent
      .builder[Props]("TargetBody")
      .initialStateFromProps(stateFromProps)
      .renderBackend[Backend]
      // .componentDidMount(_.backend.onMount)
      .componentWillReceiveProps($ => $.backend.newProps($.currentProps, $.nextProps))
      .build

}
