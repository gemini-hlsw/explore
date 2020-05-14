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
import explore.model.ModelOptics
import explore.model.SiderealTarget
import explore.target.TargetQueries._
import gem.Observation
import gsp.math.Angle
import gsp.math.Coordinates
import gsp.math.Declination
import gsp.math.HourAngle
import gsp.math.RightAscension
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import monocle.Lens
import monocle.macros.Lenses
import react.aladin.Aladin
import react.common._
import react.semanticui.collections.grid._
import react.semanticui.widths._
import gsp.math.ProperMotion

final case class TargetBody(
  observationId:    Observation.Id,
  target:           View[IO, SiderealTarget]
)(implicit val ctx: AppContextIO)
    extends ReactProps {
  @inline override def render: VdomElement = TargetBody.component(this)
  val aladinCoords: Coordinates            = target.get.track.baseCoordinates
  val aladinCoordsStr: String              = Coordinates.fromHmsDms.reverseGet(aladinCoords)
}

object TargetBody extends ModelOptics {
  type Props = TargetBody
  val AladinComp = Aladin.component

  class Backend(bs: BackendScope[Props, Unit]) {
    // Create a mutable reference
    private val ref = Ref.toScalaComponent(AladinComp)

    private val raLens: Lens[SiderealTarget, RightAscension] =
      SiderealTarget.track ^|-> ProperMotion.baseCoordinates ^|-> Coordinates.rightAscension

    private val decLens: Lens[SiderealTarget, Declination] =
      SiderealTarget.track ^|-> ProperMotion.baseCoordinates ^|-> Coordinates.declination

    def setRa(ra: RightAscension): Callback =
      bs.props >>= (_.target.zoomL(raLens).set(ra).runInCB)

    def setDec(dec: Declination): Callback =
      bs.props >>= (_.target.zoomL(decLens).set(dec).runInCB)

    private def coordinatesKey(target: SiderealTarget): String =
      s"${target.name}#${target.track.baseCoordinates.show}"

    def render(props: Props) = {
      implicit val appCtx = props.ctx

      val target = props.target.get
      UndoRegion[SiderealTarget] { undoCtx =>
        val modifyIO    =
          Modify(props.observationId, target, props.target.mod, undoCtx.setter)
        def modify[A](
          lens:   Lens[SiderealTarget, A],
          fields: A => Mutation.Fields
        ): A => Callback = { v: A =>
          modifyIO(lens.get, lens.set, fields)(v).runInCB
        }
        val gotoRaDec   = (coords: Coordinates) =>
          ref.get
            .flatMapCB(
              _.backend
                .gotoRaDec(coords.ra.toAngle.toDoubleDegrees, coords.dec.toAngle.toDoubleDegrees)
            )
            .toCallback
        val searchAndGo = (search: String) =>
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
                    setRa(ra) *> setDec(dec) *> modify[
                      (String, RightAscension, Declination)
                    ](
                      targetPropsL,
                      {
                        case (n, r, d) =>
                          Mutation.Fields(
                            name = n.some,
                            ra = RightAscension.fromStringHMS.reverseGet(r).some,
                            dec = Declination.fromStringSignedDMS.reverseGet(d).some
                          )
                      }
                    )((search, ra, dec))
                  },
                  Callback.log("error")
                )
            )
            .toCallback

        <.div(
          ^.height := "100%",
          ^.width := "100%",
          ^.cls := "check",
          Grid(columns = Two, stretched = true, padded = GridPadded.Horizontally)(
            ^.height := "100%",
            GridRow(stretched = true)(
              GridColumn(stretched = true, computer = Four, clazz = GPPStyles.GPPForm)(
                CoordinatesForm.component.withKey(coordinatesKey(props.target.get))(
                  CoordinatesForm(props.target.get, searchAndGo, gotoRaDec, undoCtx)
                )
              ),
              GridColumn(stretched = true, computer = Nine)(
                AladinComp.withRef(ref) {
                  Aladin(target = props.aladinCoordsStr, fov = 0.25, showGotoControl = false)
                }
              ),
              GridColumn(stretched = true, computer = Three, clazz = GPPStyles.GPPForm)(
                CataloguesForm(props.target.get)
              )
            )
          )
        )
      }
    }

    def newProps(currentProps: Props, nextProps: Props): Callback =
      Callback.log(currentProps.toString()) *>
        ref.get
          .flatMapCB { r =>
            val c = nextProps.aladinCoords
            r.backend.gotoRaDec(c.ra.toAngle.toDoubleDegrees, c.dec.toAngle.toDoubleDegrees)
          }
          .when(nextProps.aladinCoords =!= currentProps.aladinCoords)
  }

  val component =
    ScalaComponent
      .builder[Props]
      .renderBackend[Backend]
      .componentDidUpdate($ => $.backend.newProps($.prevProps, $.currentProps))
      .build

}
