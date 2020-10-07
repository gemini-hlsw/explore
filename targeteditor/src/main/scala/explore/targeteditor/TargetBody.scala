// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.syntax.all._
import crystal.react.implicits._
import eu.timepit.refined.auto._
import eu.timepit.refined.types.string._
import explore.AppCtx
import explore.GraphQLSchemas.ObservationDB.Types._
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
import lucuma.core.model.Target
import react.common._

final case class TargetBody(
  id:      SiderealTarget.Id,
  target:  View[SiderealTarget],
  options: View[TargetVisualOptions]
) extends ReactProps[TargetBody](TargetBody.component) {
  val aladinCoords: Coordinates = target.get.track.baseCoordinates
}

object TargetBody extends ModelOptics {
  type Props          = TargetBody
  // search term, after, on empty, on error
  type SearchCallback = (NonEmptyString, Callback, Callback, Throwable => Callback) => Callback

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

          import japgolly.scalajs.react.effects.AsyncCallbackEffects._
          val searchAndSet: SearchCallback =
            (
              term:       NonEmptyString,
              onComplete: Callback,
              onEmpty:    Callback,
              onError:    Throwable => Callback
            ) =>
              SimbadSearch
                .search(term)
                .attempt
                .runInCBAndThen {
                  case Right(Some(Target(n, Right(st), _))) =>
                    modify((n, st.baseCoordinates.ra, st.baseCoordinates.dec)).runInCB *>
                      gotoRaDec(st.baseCoordinates) *> onComplete
                  case Right(Some(r))                       => Callback.log(s"Unknown target type $r")
                  case Right(None)                          => onEmpty
                  case Left(t)                              => onError(t)
                }

          React.Fragment(
            <.div(
              ExploreStyles.TargetGrid,
              <.div(
                CoordinatesForm(target, searchAndSet, gotoRaDec)
                  .withKey(coordinatesKey(target)),
                UndoButtons(target, undoCtx)
              ),
              <.div(
                ExploreStyles.TargetAladinCell,
                AladinRef.withRef(aladinRef) {
                  AladinContainer(props.target, props.options.get)
                }
              ),
              CataloguesForm(props.options)
            ),
            <.div(
              ExploreStyles.TargetSkyplotCell,
              WIP(
                SkyPlotSection(target.track.baseCoordinates)
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
