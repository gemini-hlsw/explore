// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.implicits._
import crystal.react.implicits._
import explore.AppCtx
import explore.View
import explore.components.ui.GPPStyles
import explore.components.undo.UndoButtons
import explore.components.undo.UndoRegion
import explore.implicits._
import explore.model.ModelOptics
import explore.model.SiderealTarget
import explore.model.reusability._
import explore.target.TargetQueries._
import gsp.math.Angle
import gsp.math.Coordinates
import gsp.math.Declination
import gsp.math.HourAngle
import gsp.math.ProperMotion
import gsp.math.RightAscension
import gsp.math.geom.jts.interpreter._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import monocle.Lens
import react.aladin.JsAladin
import react.aladin.Aladin
import react.aladin.PixelScale
import react.aladin.visualization
import react.common._
import react.semanticui.collections.grid._
import react.semanticui.widths._
import org.scalajs.dom.raw.Element
import org.scalajs.dom.document

final case class TargetBody(
  id:     SiderealTarget.Id,
  target: View[SiderealTarget]
) extends ReactProps[TargetBody](TargetBody.component) {
  val aladinCoords: Coordinates = target.get.track.baseCoordinates
  val aladinCoordsStr: String   = Coordinates.fromHmsDms.reverseGet(aladinCoords)
}

object TargetBody extends ModelOptics {
  type Props = TargetBody

  protected implicit val propsReuse: Reusability[Props] = Reusability.derive

  final case class State(fov: Boolean)

  object State {
    val Default = State(false)
  }

  val AladinComp = Aladin.component

  class Backend(bs: BackendScope[Props, State]) {
    // Create a mutable reference
    private val aladinRef = Ref.toScalaComponent(AladinComp)

    private val raLens: Lens[SiderealTarget, RightAscension] =
      SiderealTarget.track ^|-> ProperMotion.baseCoordinates ^|-> Coordinates.rightAscension

    private val decLens: Lens[SiderealTarget, Declination] =
      SiderealTarget.track ^|-> ProperMotion.baseCoordinates ^|-> Coordinates.declination

    def setName(name: String): Callback =
      bs.props >>= (_.target.zoom(SiderealTarget.name).set(name).runInCB)

    def setRa(ra: RightAscension): Callback =
      bs.props >>= (_.target.zoom(raLens).set(ra).runInCB)

    def setDec(dec: Declination): Callback =
      bs.props >>= (_.target.zoom(decLens).set(dec).runInCB)

    private def coordinatesKey(target: SiderealTarget): String =
      s"${target.name}#${target.track.baseCoordinates.show}"

    val gotoRaDec = (coords: Coordinates) =>
      aladinRef.get
        .flatMapCB(
          _.backend
            .gotoRaDec(coords.ra.toAngle.toDoubleDegrees, coords.dec.toAngle.toDoubleDegrees)
        )
        .toCallback

    def searchAndGo(modify: ((String, RightAscension, Declination)) => Callback)(search: String) =
      aladinRef.get
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
                setRa(ra) *> setDec(dec) *> modify((search, ra, dec))
              },
              Callback.log("error")
            )
        )
        .toCallback

    def setTargetByName: String => Callback =
      searchAndGo { case (name, _, _) => setName(name) }

    def renderVisualization(div: Element, size: Size, pixelScale: => PixelScale): Callback =
      Callback {
        // Delete any viz previously rendered
        val previous = Option(div.querySelector(".aladin-visualization"))
        previous.foreach(div.removeChild)
        val g        = document.createElement("div")
        g.classList.add("aladin-visualization")
        visualization.geometryForAladin(GmosGeometry.shapes(GmosGeometry.posAngle),
                                        g,
                                        size,
                                        pixelScale,
                                        GmosGeometry.ScaleFactor
        )
        div.appendChild(g)
      }

    def includeSvg(v: JsAladin): Unit = {
      val size = Size(v.getParentDiv().clientHeight, v.getParentDiv().clientWidth)
      val div  = v.getParentDiv()
      v.onZoomCB(renderVisualization(div, size, v.pixelScale))
      ()
    }

    def updateVisualization(v: JsAladin): Callback = {
      val size = Size(v.getParentDiv().clientHeight, v.getParentDiv().clientWidth)
      val div  = v.getParentDiv()
      renderVisualization(div, size, v.pixelScale)
    }

    def render(props: Props) =
      AppCtx.withCtx { implicit appCtx =>
        val target = props.target.get

        UndoRegion[SiderealTarget] { undoCtx =>
          val undoSet =
            UndoSet(props.id, props.target, undoCtx.setter)

          val modify = undoSet[
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
          ) _

          val searchAndSet: String => Callback =
            searchAndGo(modify.andThen(_.runInCB))

          <.div(
            ^.height := "100%",
            ^.width := "100%",
            ^.cls := "check",
            Grid(columns = Two, stretched = true, padded = GridPadded.Horizontally)(
              ^.height := "100%",
              GridRow(stretched = true)(
                GridColumn(stretched = true, computer = Four, clazz = GPPStyles.GPPForm)(
                  CoordinatesForm(target, searchAndSet, gotoRaDec)
                    .withKey(coordinatesKey(target)),
                  UndoButtons(target, undoCtx)
                ),
                GridColumn(stretched = true, computer = Nine)(
                  AladinComp.withRef(aladinRef) {
                    Aladin(target = props.aladinCoordsStr,
                           fov = 0.25,
                           showGotoControl = false,
                           customize = includeSvg _
                    )
                  }
                ),
                GridColumn(stretched = true, computer = Three, clazz = GPPStyles.GPPForm)(
                  CataloguesForm(true)
                )
              )
            )
          )
        }
      }

    def newProps(currentProps: Props, nextProps: Props): Callback =
      // Callback.log(currentProps.toString()) *>
      aladinRef.get
        .flatMapCB { r =>
          val c = nextProps.aladinCoords
          r.backend.gotoRaDec(c.ra.toAngle.toDoubleDegrees, c.dec.toAngle.toDoubleDegrees)
        }
        .when(nextProps.aladinCoords =!= currentProps.aladinCoords)
  }

  val component =
    ScalaComponent
      .builder[Props]
      .initialState(State.Default)
      .renderBackend[Backend]
      .componentDidUpdate($ => $.backend.newProps($.prevProps, $.currentProps))
      .build

}
