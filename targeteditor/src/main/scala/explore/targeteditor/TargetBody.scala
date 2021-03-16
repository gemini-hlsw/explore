// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.effect.IO
import cats.syntax.all._
import clue.data.syntax._
import crystal.react.implicits._
import eu.timepit.refined.auto._
import eu.timepit.refined.types.string._
import explore.AppCtx
import explore.GraphQLSchemas.ObservationDB.Types._
import explore.View
import explore.common.SimbadSearch
import explore.common.TargetQueries
import explore.common.TargetQueries._
import explore.components.WIP
import explore.components.ui.ExploreStyles
import explore.components.undo.UndoButtons
import explore.components.undo.UndoRegion
import explore.implicits._
import explore.model.TargetVisualOptions
import explore.model.formats._
import explore.model.reusability._
import explore.model.utils._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.math._
import lucuma.core.model.Magnitude
import lucuma.core.model.SiderealTracking
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.ui.forms.FormInputEV
import lucuma.ui.implicits._
import lucuma.ui.optics.ChangeAuditor
import lucuma.ui.optics.TruncatedDec
import lucuma.ui.optics.TruncatedRA
import lucuma.ui.optics.ValidFormatInput
import lucuma.ui.reusability._
import react.common._
import react.common.implicits._
import react.semanticui.collections.form.Form
import react.semanticui.elements.label.LabelPointing
import react.semanticui.sizes.Small

final case class SearchCallback(
  searchTerm: NonEmptyString,
  onComplete: Option[Target] => Callback,
  onError:    Throwable => Callback
) {
  def run: Callback = Callback.empty
}

final case class TargetBody(
  uid:       User.Id,
  id:        Target.Id,
  target:    View[TargetResult],
  searching: View[Set[Target.Id]],
  options:   View[TargetVisualOptions]
) extends ReactProps[TargetBody](TargetBody.component) {
  val baseCoordinates: Coordinates =
    target.zoom(TargetQueries.baseCoordinates).get
}

object TargetBody {

  type Props = TargetBody
  val AladinRef = AladinCell.component

  implicit val propsReuse = Reusability.derive[Props]

  class Backend() {
    def render(props: Props) =
      AppCtx.withCtx { implicit appCtx =>
        val target = props.target.get

        UndoRegion[TargetResult] { undoCtx =>
          val undoViewSet =
            UndoView(props.id, props.target, undoCtx.setter)

          val allView = undoViewSet(
            targetPropsL,
            { args: (NonEmptyString, SiderealTracking, List[Magnitude]) =>
              EditSiderealInput.name.set(args._1.value.assign) >>>
                TargetQueries.UpdateSiderealTracking(args._2) >>>
                TargetQueries.updateMagnitudes(args._3)
            }
          )

          val coordsRAView = undoViewSet(
            TargetQueries.baseCoordinatesRa,
            (TargetQueries.UpdateSiderealTracking.ra _).compose((_: RightAscension).some)
          )

          val coordsDecView = undoViewSet(
            TargetQueries.baseCoordinatesDec,
            (TargetQueries.UpdateSiderealTracking.dec _).compose((_: Declination).some)
          )

          val epochView =
            undoViewSet(
              TargetQueries.epoch,
              (TargetQueries.UpdateSiderealTracking.epoch _).compose((_: Epoch).some)
            )

          val magnitudesView =
            undoViewSet(TargetResult.magnitudes, TargetQueries.updateMagnitudes)

          val nameView = undoViewSet(
            TargetResult.name,
            (EditSiderealInput.name.set _).compose((_: NonEmptyString).value.assign)
          )

          val properMotionRAView = undoViewSet(
            TargetQueries.pmRALens,
            (pmRA: Option[ProperMotion.RA]) =>
              TargetQueries.UpdateSiderealTracking.properMotion(
                buildProperMotion(pmRA, TargetQueries.pmDecLens.get(target))
              )
          )

          val properMotionDecView = undoViewSet(
            TargetQueries.pmDecLens,
            (pmDec: Option[ProperMotion.Dec]) =>
              TargetQueries.UpdateSiderealTracking.properMotion(
                buildProperMotion(TargetQueries.pmRALens.get(target), pmDec)
              )
          )

          val parallaxView = undoViewSet(
            TargetQueries.pxLens,
            TargetQueries.UpdateSiderealTracking.parallax
          )

          val radialVelocityView = undoViewSet(
            TargetQueries.rvLens,
            TargetQueries.UpdateSiderealTracking.radialVelocity
          )

          val searchAndSet: SearchCallback => Callback = s =>
            SimbadSearch
              .search(s.searchTerm)
              .attempt
              .runAsyncAndThenCB {
                case Right(r @ Some(Target(n, Right(st), m))) =>
                  allView.set((n, st, m.values.toList)).runAsyncCB >>
                    s.onComplete(r)
                case Right(Some(r))                           =>
                  Callback.log(s"Unknown target type $r") >>
                    nameView.set(s.searchTerm).runAsyncCB >>
                    s.onComplete(none)
                case Right(None)                              =>
                  nameView.set(s.searchTerm).runAsyncCB >>
                    s.onComplete(none)
                case Left(t)                                  =>
                  nameView.set(s.searchTerm).runAsyncCB >>
                    s.onError(t)
              }

          val disabled = props.searching.get.exists(_ === props.id)

          React.Fragment(
            <.div(
              ExploreStyles.TargetGrid,
              Form(as = <.div, size = Small)(
                ExploreStyles.Grid,
                ExploreStyles.Compact,
                // Keep the search field and the coords always together
                SearchForm(
                  props.id,
                  // SearchForm doesn't edit the name directly. It will set it atomically, together
                  // with coords & magnitudes from the catalog search, so that all 3 fields are
                  // a single undo/redo operation.
                  props.target.zoom(TargetResult.name).get,
                  props.searching,
                  searchAndSet
                ),
                <.div(
                  ExploreStyles.FlexContainer,
                  ExploreStyles.TargetRaDecMinWidth,
                  FormInputEV(
                    id = "ra",
                    value = coordsRAView.zoomSplitEpi(TruncatedRA.rightAscension),
                    validFormat = ValidFormatInput.truncatedRA,
                    changeAuditor = ChangeAuditor.truncatedRA,
                    label = "RA",
                    clazz = ExploreStyles.FlexGrow(1) |+| ExploreStyles.TargetRaDecMinWidth,
                    errorPointing = LabelPointing.Below,
                    errorClazz = ExploreStyles.InputErrorTooltip,
                    disabled = disabled
                  ),
                  FormInputEV(
                    id = "dec",
                    value = coordsDecView.zoomSplitEpi(TruncatedDec.declination),
                    validFormat = ValidFormatInput.truncatedDec,
                    changeAuditor = ChangeAuditor.truncatedDec,
                    label = "Dec",
                    clazz = ExploreStyles.FlexGrow(1) |+| ExploreStyles.TargetRaDecMinWidth,
                    errorPointing = LabelPointing.Below,
                    errorClazz = ExploreStyles.InputErrorTooltip,
                    disabled = disabled
                  )
                )
              ),
              AladinCell(
                props.uid,
                props.target.get.id,
                props.target.zoom(TargetQueries.baseCoordinates),
                props.options
              ),
              CataloguesForm(props.options).when(false),
              Form(as = <.div, size = Small)(
                ExploreStyles.Grid,
                ExploreStyles.Compact,
                ExploreStyles.TargetPropertiesForm,
                InputWithUnits(
                  epochView,
                  ValidFormatInput.fromFormat(Epoch.fromStringNoScheme, "Invalid Epoch"),
                  ChangeAuditor.maxLength(8).decimal(3).deny("-").as[Epoch],
                  id = "epoch",
                  label = "Epoch",
                  units = "years",
                  disabled = disabled
                ),
                InputWithUnits(
                  properMotionRAView,
                  ValidFormatInput.fromFormatOptional(pmRAFormat, "Must be a number"),
                  ChangeAuditor.fromFormat(pmRAFormat).decimal(3).optional,
                  id = "raPM",
                  label = "µ RA",
                  units = "mas/y",
                  disabled = disabled
                ),
                InputWithUnits(
                  properMotionDecView,
                  ValidFormatInput.fromFormatOptional(pmDecFormat, "Must be a number"),
                  ChangeAuditor.fromFormat(pmDecFormat).decimal(3).optional,
                  id = "raDec",
                  label = "µ Dec",
                  units = "mas/y",
                  disabled = disabled
                ),
                InputWithUnits[IO, Option[Parallax]](
                  parallaxView,
                  ValidFormatInput.fromFormatOptional(pxFormat, "Must be a number"),
                  ChangeAuditor.fromFormat(pxFormat).decimal(3).optional,
                  id = "parallax",
                  label = "Parallax",
                  units = "mas",
                  disabled = disabled
                ),
                RVInput(radialVelocityView, disabled)
              ),
              MagnitudeForm(target.id, magnitudesView, disabled = disabled),
              UndoButtons(target, undoCtx, disabled = disabled),
              <.div(
                ExploreStyles.TargetSkyplotCell,
                WIP(
                  SkyPlotSection(props.baseCoordinates)
                ).when(false)
              )
            )
          )
        }
      }
  }

  val component =
    ScalaComponent
      .builder[Props]
      .renderBackend[Backend]
      .configure(Reusability.shouldComponentUpdate)
      .build

}
