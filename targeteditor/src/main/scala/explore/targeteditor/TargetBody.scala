// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.syntax.all._
import crystal.react.implicits._
import eu.timepit.refined._
import eu.timepit.refined.collection._
import eu.timepit.refined.types.string._
import explore.AppCtx
import explore.View
import explore.components.WIP
import explore.components.ui.ExploreStyles
import explore.components.undo.UndoButtons
import explore.components.undo.UndoRegion
import explore.implicits._
import explore.model.ModelOptics
import explore.model.SiderealTarget
import explore.model.TargetVisualOptions
import explore.model.reusability._
import explore.target.TargetQueries._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.RightAscension
import react.common._
import react.semanticui.collections.grid._
import react.semanticui.widths._
import react.sizeme.SizeMe
import explore.GraphQLSchemas.ObservationDB.Types._

final case class TargetBody(
  id:      SiderealTarget.Id,
  target:  View[SiderealTarget],
  options: View[TargetVisualOptions]
) extends ReactProps[TargetBody](TargetBody.component) {
  val aladinCoords: Coordinates = target.get.track.baseCoordinates
}

object TargetBody extends ModelOptics {
  type Props = TargetBody

  val AladinRef = AladinContainer.component

  implicit val propsReuse = Reusability.derive[Props]

  class Backend(bs: BackendScope[Props, Unit]) {
    // Create a mutable reference
    private val aladinRef = Ref.toScalaComponent(AladinRef)

    def setName(name: NonEmptyString): Callback =
      bs.props >>= (_.target.zoom(SiderealTarget.name).set(name).runInCB)

    private def coordinatesKey(target: SiderealTarget): String =
      s"${target.name.value}#${target.track.baseCoordinates.show}"

    val gotoRaDec = (coords: Coordinates) =>
      aladinRef.get
        .flatMapCB(_.backend.gotoRaDec(coords))
        .toCallback

    def searchAndGo(
      modify: ((NonEmptyString, RightAscension, Declination)) => Callback
    )(search: NonEmptyString) = {
      val aladinModify = (s: String, r: RightAscension, d: Declination) =>
        refineV[NonEmpty](s).fold(_ => Callback.empty, s => modify((s, r, d)))
      aladinRef.get
        .flatMapCB(
          _.backend.searchAndGo(Function.tupled(aladinModify))(search.value)
        )
        .toCallback
    }

    def setTargetByName: NonEmptyString => Callback =
      searchAndGo { case (name, _, _) => setName(name) }

    def render(props: Props) =
      AppCtx.withCtx { implicit appCtx =>
        val target = props.target.get

        UndoRegion[SiderealTarget] { undoCtx =>
          val undoSet =
            UndoSet(props.id, props.target, undoCtx.setter)

          val modify = undoSet[
            (NonEmptyString, RightAscension, Declination)
          ](
            targetPropsL,
            { case (n, r, d) =>
              TargetsSetInput(
                name = n.value.some,
                ra = RightAscension.fromStringHMS.reverseGet(r).some,
                dec = Declination.fromStringSignedDMS.reverseGet(d).some
              )
            }
          ) _

          val searchAndSet: NonEmptyString => Callback =
            searchAndGo(modify.andThen(_.runInCB))

          Grid(columns = Three,
               clazz = ExploreStyles.FullHeightWidth,
               stretched = true,
               padded = GridPadded.Horizontally
          )(
            GridRow(stretched = true)(
              GridColumn(stretched = true, computer = Four)(
                CoordinatesForm(target, searchAndSet, gotoRaDec)
                  .withKey(coordinatesKey(target)),
                UndoButtons(target, undoCtx)
              ),
              GridColumn(stretched = true, computer = Eight, clazz = ExploreStyles.AladinColumn)(
                SizeMe(monitorHeight = true) { s =>
                  AladinRef.withRef(aladinRef) {
                    AladinContainer(s, props.target, props.options.get)
                  }
                }
              ),
              GridColumn(stretched = true, computer = Four)(
                CataloguesForm(props.options)
              )
            ),
            GridRow()(
              GridColumn(computer = Sixteen)(
                WIP(
                  SkyPlotSection(target.track.baseCoordinates)
                )
              )
            )
          )
        }
      }

    def newProps(currentProps: Props, nextProps: Props): Callback =
      gotoRaDec(nextProps.aladinCoords)
        .when(nextProps.aladinCoords =!= currentProps.aladinCoords)
        .void
  }

  val component =
    ScalaComponent
      .builder[Props]
      .renderBackend[Backend]
      .componentDidUpdate($ => $.backend.newProps($.prevProps, $.currentProps))
      .configure(Reusability.shouldComponentUpdate)
      .build

}
