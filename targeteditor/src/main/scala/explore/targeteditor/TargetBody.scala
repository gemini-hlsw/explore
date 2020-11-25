// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.effect.IO
import cats.syntax.all._
import crystal.ViewF
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
import explore.model.TargetVisualOptions
import explore.model.formats._
import explore.model.reusability._
import explore.model.utils._
import explore.target.TargetQueries
import explore.target.TargetQueries._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.math._
import lucuma.core.model.Magnitude
import lucuma.core.model.SiderealTracking
import lucuma.core.model.Target
import lucuma.ui.forms.FormInputEV
import lucuma.ui.optics.ChangeAuditor
import lucuma.ui.optics.ValidFormatInput
import lucuma.ui.reusability._
import monocle.macros.Lenses
import react.common._
import react.common.implicits._
import react.semanticui.collections.form.Form
import react.semanticui.sizes.Small

final case class SearchCallback(
  searchTerm: NonEmptyString,
  onComplete: Option[Target] => Callback,
  onError:    Throwable => Callback
) {
  def run: Callback = Callback.empty
}

final case class TargetBody(
  id:      Target.Id,
  target:  View[TargetResult],
  options: View[TargetVisualOptions]
) extends ReactProps[TargetBody](TargetBody.component) {
  val baseCoordinates: Coordinates =
    target.zoom(TargetQueries.baseCoordinates).get
}

object TargetBody {

  type Props = TargetBody
  val AladinRef = AladinCell.component

  @Lenses
  final case class State(searching: Boolean)

  implicit val propsReuse = Reusability.derive[Props]
  implicit val stateReuse = Reusability.derive[State]

  class Backend($ : BackendScope[Props, State]) {
    def render(props: Props) =
      AppCtx.withCtx { implicit appCtx =>
        val target    = props.target.get
        val stateView = ViewF.fromState[IO]($).zoom(State.searching)

        UndoRegion[TargetResult] { undoCtx =>
          val undoSet =
            UndoSet(props.id, props.target, undoCtx.setter)

          val modify = undoSet[
            (NonEmptyString, SiderealTracking, List[Magnitude])
          ](
            targetPropsL,
            { case (n, t, _ /*ms*/ ) =>
              EditSiderealInput.name.set(n.value.some) >>> TargetQueries.UpdateSiderealTracking(t)
            // >>> EditSiderealInput.magnitudes.set( ms.map(m =>
            //        MagnitudeInput(m.value.toBigDecimal, m.band, none, m.system.some)
            //      ))
            }
          ) _

          val modifyCoordsRa = undoSet[RightAscension](
            TargetQueries.baseCoordinatesRa,
            r => TargetQueries.UpdateSiderealTracking.ra(r.some)
          ) _

          val modifyCoordsDec = undoSet[Declination](
            TargetQueries.baseCoordinatesDec,
            d => TargetQueries.UpdateSiderealTracking.dec(d.some)
          ) _

          val modifyName = undoSet[NonEmptyString](
            unsafeTargetName,
            n => EditSiderealInput.name.set(n.value.some)
          ) _

          val modifyEpoch = undoSet[Epoch](
            TargetQueries.epoch,
            e => TargetQueries.UpdateSiderealTracking.epoch(e.some)
          ) _

          val modifyProperMotionRA = undoSet[Option[ProperMotion.RA]](
            TargetQueries.pmRALens,
            pmRA =>
              TargetQueries.UpdateSiderealTracking.properMotion(
                buildProperMotion(pmRA, TargetQueries.pmDecLens.get(target))
              )
          ) _

          val modifyProperMotionDec = undoSet[Option[ProperMotion.Dec]](
            TargetQueries.pmDecLens,
            pmDec =>
              TargetQueries.UpdateSiderealTracking.properMotion(
                buildProperMotion(TargetQueries.pmRALens.get(target), pmDec)
              )
          ) _

          val modifyParallax = undoSet[Option[Parallax]](
            TargetQueries.pxLens,
            TargetQueries.UpdateSiderealTracking.parallax
          ) _

          val modifyRadialVelocity = undoSet[Option[RadialVelocity]](
            TargetQueries.rvLens,
            TargetQueries.UpdateSiderealTracking.radialVelocity
          ) _

          val searchAndSet: SearchCallback => Callback =
            s =>
              SimbadSearch
                .search(s.searchTerm)
                .attempt
                .runAsyncAndThenCB {
                  case Right(r @ Some(Target(n, Right(st), m))) =>
                    modify((n, st, m.values.toList)).runAsyncCB *> s.onComplete(r)
                  case Right(Some(r))                           =>
                    Callback.log(s"Unknown target type $r") *> s.onComplete(none)
                  case Right(None)                              => s.onComplete(none)
                  case Left(t)                                  => s.onError(t)
                }

          React.Fragment(
            <.div(
              ExploreStyles.TargetGrid,
              <.div(
                SearchForm(
                  props.target.zoom(TargetQueries.unsafeTargetName).withOnMod(modifyName),
                  stateView,
                  searchAndSet
                ),
                Form(size = Small)(
                  ExploreStyles.Grid,
                  ExploreStyles.Compact,
                  <.div(
                    ExploreStyles.FlexContainer,
                    ExploreStyles.TargetRaDecMinWidth,
                    FormInputEV(
                      id = "ra",
                      value = props.target
                        .zoom(TargetQueries.baseCoordinatesRa)
                        .withOnMod(modifyCoordsRa),
                      validFormat = ValidFormatInput.fromFormat(RightAscension.fromStringHMS),
                      changeAuditor = ChangeAuditor.rightAscension,
                      label = "RA",
                      clazz = ExploreStyles.FlexGrow(1) |+| ExploreStyles.TargetRaDecMinWidth,
                      disabled = stateView.get
                    ),
                    FormInputEV(
                      id = "dec",
                      value = props.target
                        .zoom(TargetQueries.baseCoordinatesDec)
                        .withOnMod(modifyCoordsDec),
                      validFormat = ValidFormatInput.fromFormat(Declination.fromStringSignedDMS),
                      changeAuditor = ChangeAuditor.declination,
                      label = "Dec",
                      clazz = ExploreStyles.FlexGrow(1) |+| ExploreStyles.TargetRaDecMinWidth,
                      disabled = stateView.get
                    )
                  ),
                  <.div(
                    ExploreStyles.Grid,
                    ExploreStyles.Compact,
                    ExploreStyles.TargetPropertiesForm,
                    InputWithUnits(
                      props.target.zoom(TargetQueries.epoch).withOnMod(modifyEpoch),
                      ValidFormatInput.fromFormat(Epoch.fromStringNoScheme, "Must be a number"),
                      ChangeAuditor.fromFormat(Epoch.fromStringNoScheme).decimal(3),
                      id = "epoch",
                      label = "Epoch",
                      units = "years",
                      disabled = stateView
                    ),
                    InputWithUnits(
                      props.target.zoom(TargetQueries.pmRALens).withOnMod(modifyProperMotionRA),
                      ValidFormatInput.fromFormatOptional(pmRAFormat, "Must be a number"),
                      ChangeAuditor.fromFormat(pmRAFormat).decimal(3).optional,
                      id = "raPM",
                      label = "µ RA",
                      units = "mas/y",
                      disabled = stateView
                    ),
                    InputWithUnits(
                      props.target.zoom(TargetQueries.pmDecLens).withOnMod(modifyProperMotionDec),
                      ValidFormatInput.fromFormatOptional(pmDecFormat, "Must be a number"),
                      ChangeAuditor.fromFormat(pmDecFormat).decimal(3).optional,
                      id = "raDec",
                      label = "µ Dec",
                      units = "mas/y",
                      disabled = stateView
                    ),
                    InputWithUnits[cats.effect.IO, Option[Parallax]](
                      props.target.zoom(TargetQueries.pxLens).withOnMod(modifyParallax),
                      ValidFormatInput.fromFormatOptional(pxFormat, "Must be a number"),
                      ChangeAuditor.fromFormat(pxFormat).decimal(3).optional,
                      id = "parallax",
                      label = "Parallax",
                      units = "mas",
                      disabled = stateView
                    ),
                    RVInput(
                      props.target.zoom(TargetQueries.rvLens).withOnMod(modifyRadialVelocity),
                      stateView
                    )
                  )
                ),
                MagnitudeForm(target.magnitudes).when(false),
                UndoButtons(target, undoCtx)
              ),
              AladinCell(
                props.target.zoom(TargetQueries.baseCoordinates),
                props.options
              ),
              CataloguesForm(props.options).when(false)
            ),
            <.div(
              ExploreStyles.TargetSkyplotCell,
              WIP(
                SkyPlotSection(props.baseCoordinates)
              ).when(false)
            )
          )
        }
      }
  }

  val component =
    ScalaComponent
      .builder[Props]
      .initialState(State(false))
      .renderBackend[Backend]
      .configure(Reusability.shouldComponentUpdate)
      .build

}
