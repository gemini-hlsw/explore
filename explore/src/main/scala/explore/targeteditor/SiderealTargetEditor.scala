// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.Endo
import cats.effect.IO
import cats.syntax.all._
import clue.data.syntax._
import crystal.react.View
import crystal.react.implicits._
import crystal.react.reuse._
import eu.timepit.refined.auto._
import eu.timepit.refined.types.string._
import explore.AppCtx
import explore.common.SimbadSearch
import explore.common.TargetQueries
import explore.common.TargetQueries._
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.components.undo.UndoButtons
import explore.implicits._
import explore.model.TargetVisualOptions
import explore.model.formats._
import explore.model.reusability._
import explore.model.util._
import explore.undo.UndoContext
import explore.undo.UndoStacks
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.math._
import lucuma.core.model.SiderealTarget
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.schemas.ObservationDB.Types._
import lucuma.ui.forms.FormInputEV
import lucuma.ui.implicits._
import lucuma.ui.optics.ChangeAuditor
import lucuma.ui.optics.TruncatedDec
import lucuma.ui.optics.TruncatedRA
import lucuma.ui.optics.ValidFormatInput
import lucuma.ui.reusability._
import monocle.Iso
import react.common._
import react.semanticui.collections.form.Form
import react.semanticui.elements.divider._
import react.semanticui.elements.label.LabelPointing
import react.semanticui.sizes.Small

final case class SearchCallback(
  searchTerm: NonEmptyString,
  onComplete: Option[Target] => Callback,
  onError:    Throwable => Callback
) {
  def run: Callback = Callback.empty
}

final case class SiderealTargetEditor(
  uid:        User.Id,
  id:         Target.Id,
  target:     View[SiderealTarget],
  undoStacks: View[UndoStacks[IO, SiderealTarget]],
  searching:  View[Set[Target.Id]],
  options:    View[TargetVisualOptions]
) extends ReactFnProps[SiderealTargetEditor](SiderealTargetEditor.component) {
  val baseCoordinates: Coordinates =
    target.zoom(SiderealTarget.baseCoordinates).get
}

object SiderealTargetEditor {

  type Props = SiderealTargetEditor

  implicit val propsReuse: Reusability[Props] = Reusability.derive

  val component =
    ScalaFnComponent
      .withReuse[Props] { props =>
        AppCtx.using { implicit appCtx =>
          val undoCtx     = UndoContext(props.undoStacks, props.target)
          val target      = props.target.get
          val undoViewSet = UndoView(props.id, undoCtx)

          val allView = undoViewSet(
            Iso.id.asLens,
            { t: SiderealTarget =>
              EditTargetInput.name.replace(t.name.assign) >>>
                TargetQueries.UpdateSiderealTracking(t.tracking) >>>
                TargetQueries.replaceMagnitudes(t.magnitudes)
            }
          )

          val coordsRAView = undoViewSet(
            SiderealTarget.baseRA,
            (TargetQueries.UpdateSiderealTracking.ra _).compose((_: RightAscension).some)
          )

          val coordsDecView = undoViewSet(
            SiderealTarget.baseDec,
            (TargetQueries.UpdateSiderealTracking.dec _).compose((_: Declination).some)
          )

          val epochView =
            undoViewSet(
              SiderealTarget.epoch,
              (TargetQueries.UpdateSiderealTracking.epoch _).compose((_: Epoch).some)
            )

          val magnitudesView =
            undoViewSet(SiderealTarget.magnitudes, TargetQueries.replaceMagnitudes)

          val nameView = undoViewSet(
            SiderealTarget.name,
            (EditTargetInput.name.replace _).compose((_: NonEmptyString).assign)
          )

          val properMotionRAView = undoViewSet(
            SiderealTarget.properMotionRA.getOption,
            (f: Endo[Option[ProperMotion.RA]]) =>
              SiderealTarget.properMotionRA.modify(unsafeOptionFnUnlift(f)),
            (pmRA: Option[ProperMotion.RA]) =>
              TargetQueries.UpdateSiderealTracking.properMotion(
                buildProperMotion(pmRA, SiderealTarget.properMotionDec.getOption(target))
              )
          )

          val properMotionDecView = undoViewSet(
            SiderealTarget.properMotionDec.getOption,
            (f: Endo[Option[ProperMotion.Dec]]) =>
              SiderealTarget.properMotionDec.modify(unsafeOptionFnUnlift(f)),
            (pmDec: Option[ProperMotion.Dec]) =>
              TargetQueries.UpdateSiderealTracking.properMotion(
                buildProperMotion(SiderealTarget.properMotionRA.getOption(target), pmDec)
              )
          )

          val parallaxView = undoViewSet(
            SiderealTarget.parallax,
            TargetQueries.UpdateSiderealTracking.parallax
          )

          val radialVelocityView = undoViewSet(
            SiderealTarget.radialVelocity,
            TargetQueries.UpdateSiderealTracking.radialVelocity
          )

          def searchAndSet(
            allView:  View[SiderealTarget],
            nameView: View[NonEmptyString],
            s:        SearchCallback
          ): Callback =
            SimbadSearch
              .search[IO](s.searchTerm)
              .map(_.headOption)
              .runAsyncAndThen {
                case Right(r @ Some(t)) =>
                  allView.set(t) >> s.onComplete(r)
                case Right(None)        =>
                  nameView.set(s.searchTerm) >> s.onComplete(none)
                case Left(t)            =>
                  nameView.set(s.searchTerm) >> s.onError(t)
              }

          val disabled = props.searching.get.exists(_ === props.id)

          React.Fragment(
            Divider(hidden = true, fitted = true),
            <.span(ExploreStyles.TitleUndoButtons, UndoButtons(undoCtx, disabled = disabled)),
            <.div(ExploreStyles.TargetGrid)(
              <.div(ExploreStyles.Grid, ExploreStyles.Compact, ExploreStyles.TargetForm)(
                // Keep the search field and the coords always together
                SearchForm(
                  props.id,
                  // SearchForm doesn't edit the name directly. It will set it atomically, together
                  // with coords & magnitudes from the catalog search, so that all 3 fields are
                  // a single undo/redo operation.
                  props.target.zoom(SiderealTarget.name).get,
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
                props.id,
                props.target.zoom(SiderealTarget.baseCoordinates),
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
              MagnitudeForm(magnitudesView, disabled = disabled),
              <.div(ExploreStyles.TargetSkyplotCell)(
                SkyPlotSection(props.baseCoordinates)
              )
            )
          )
        }
      }

}
