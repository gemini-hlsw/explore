// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.effect.IO
import cats.syntax.all._
import clue.data.syntax._
import crystal.react.implicits._
import crystal.react.reuse._
import eu.timepit.refined.auto._
import eu.timepit.refined.types.string._
import explore.AppCtx
import explore.View
import explore.common.SimbadSearch
import explore.common.TargetQueries
import explore.common.TargetQueries._
import explore.components.HelpIcon
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.components.undo.UndoButtons
import explore.implicits._
import explore.model.TargetVisualOptions
import explore.model.formats._
import explore.model.reusability._
import explore.model.utils._
import explore.schemas.ObservationDB.Types._
import explore.undo.UndoContext
import explore.undo.UndoStacks
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
  uid:           User.Id,
  id:            Target.Id,
  target:        View[TargetResult],
  undoStacks:    View[UndoStacks[IO, TargetResult]],
  searching:     View[Set[Target.Id]],
  options:       View[TargetVisualOptions],
  renderInTitle: Tile.RenderInTitle
) extends ReactProps[TargetBody](TargetBody.component) {
  val baseCoordinates: Coordinates =
    target.zoom(TargetQueries.baseCoordinates).get
}

object TargetBody {

  type Props = TargetBody

  implicit val propsReuse: Reusability[Props] = Reusability.derive

  val AladinRef = AladinCell.component

  class Backend() {
    def render(props: Props) =
      AppCtx.using { implicit appCtx =>
        val undoCtx     = UndoContext(props.undoStacks, props.target)
        val target      = props.target.get
        val undoViewSet = UndoView(props.id, undoCtx)

        val allView = undoViewSet(
          targetPropsL,
          { args: (NonEmptyString, SiderealTracking, List[Magnitude]) =>
            EditSiderealInput.name.replace(args._1.value.assign) >>>
              TargetQueries.UpdateSiderealTracking(args._2) >>>
              TargetQueries.replaceMagnitudes(args._3)
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
          undoViewSet(TargetResult.magnitudes, TargetQueries.replaceMagnitudes)

        val nameView = undoViewSet(
          TargetResult.name,
          (EditSiderealInput.name.replace _).compose((_: NonEmptyString).value.assign)
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

        def searchAndSet(
          allView:  View[(NonEmptyString, SiderealTracking, List[Magnitude])],
          nameView: View[NonEmptyString],
          s:        SearchCallback
        ): Callback =
          SimbadSearch
            .search[IO](s.searchTerm)
            .runAsyncAndThenCB {
              case Right(r @ Some(Target(n, Right(st), m))) =>
                allView.set((n, st, m.values.toList)).toCB >> s.onComplete(r)
              case Right(Some(r))                           =>
                Callback.log(s"Unknown target type $r") >>
                  nameView.set(s.searchTerm).toCB >> s.onComplete(none)
              case Right(None)                              =>
                nameView.set(s.searchTerm).toCB >> s.onComplete(none)
              case Left(t)                                  =>
                nameView.set(s.searchTerm).toCB >> s.onError(t)
            }

        val disabled = props.searching.get.exists(_ === props.id)

        React.Fragment(
          <.div(ExploreStyles.TargetGrid)(
            <.div(ExploreStyles.Grid, ExploreStyles.Compact, ExploreStyles.TargetForm)(
              // Keep the search field and the coords always together
              SearchForm(
                props.id,
                // SearchForm doesn't edit the name directly. It will set it atomically, together
                // with coords & magnitudes from the catalog search, so that all 3 fields are
                // a single undo/redo operation.
                props.target.zoom(TargetResult.name).get,
                props.searching,
                Reuse.currying(allView, nameView).in(searchAndSet _)
              ),
              <.label("RA", HelpIcon("target/main/coordinates.md"), ExploreStyles.SkipToNext),
              FormInputEV(
                id = "ra",
                value = coordsRAView.zoomSplitEpi(TruncatedRA.rightAscension),
                validFormat = ValidFormatInput.truncatedRA,
                changeAuditor = ChangeAuditor.truncatedRA,
                clazz = ExploreStyles.TargetRaDecMinWidth,
                errorPointing = LabelPointing.Below,
                errorClazz = ExploreStyles.InputErrorTooltip,
                disabled = disabled
              ),
              <.label("Dec", HelpIcon("target/main/coordinates.md"), ExploreStyles.SkipToNext),
              FormInputEV(
                id = "dec",
                value = coordsDecView.zoomSplitEpi(TruncatedDec.declination),
                validFormat = ValidFormatInput.truncatedDec,
                changeAuditor = ChangeAuditor.truncatedDec,
                clazz = ExploreStyles.TargetRaDecMinWidth,
                errorPointing = LabelPointing.Below,
                errorClazz = ExploreStyles.InputErrorTooltip,
                disabled = disabled
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
              ExploreStyles.ExploreForm,
              <.label("Epoch", HelpIcon("target/main/epoch.md"), ExploreStyles.SkipToNext),
              InputWithUnits(
                epochView,
                ValidFormatInput.fromFormat(Epoch.fromStringNoScheme, "Invalid Epoch"),
                ChangeAuditor.maxLength(8).decimal(3).deny("-").as[Epoch],
                id = "epoch",
                units = "years",
                disabled = disabled
              ),
              <.label("µ RA", ExploreStyles.SkipToNext),
              InputWithUnits(
                properMotionRAView,
                ValidFormatInput.fromFormatOptional(pmRAFormat, "Must be a number"),
                ChangeAuditor.fromFormat(pmRAFormat).decimal(3).optional,
                id = "raPM",
                units = "mas/y",
                disabled = disabled
              ),
              <.label("µ Dec", ExploreStyles.SkipToNext),
              InputWithUnits(
                properMotionDecView,
                ValidFormatInput.fromFormatOptional(pmDecFormat, "Must be a number"),
                ChangeAuditor.fromFormat(pmDecFormat).decimal(3).optional,
                id = "raDec",
                units = "mas/y",
                disabled = disabled
              ),
              <.label("Parallax", ExploreStyles.SkipToNext),
              InputWithUnits(
                parallaxView,
                ValidFormatInput.fromFormatOptional(pxFormat, "Must be a number"),
                ChangeAuditor.fromFormat(pxFormat).decimal(3).optional,
                id = "parallax",
                units = "mas",
                disabled = disabled
              ),
              RVInput(radialVelocityView, disabled)
            ),
            MagnitudeForm(target.id, magnitudesView, disabled = disabled),
            <.div(ExploreStyles.TargetSkyplotCell)(
              SkyPlotSection(props.baseCoordinates)
            ),
            props.renderInTitle(
              <.span(ExploreStyles.TitleStrip, UndoButtons(undoCtx, disabled = disabled))
            )
          )
        )
      }
  }

  val component =
    ScalaComponent
      .builder[Props]
      .renderBackend[Backend]
      .configure(Reusability.shouldComponentUpdate)
      .build

}
