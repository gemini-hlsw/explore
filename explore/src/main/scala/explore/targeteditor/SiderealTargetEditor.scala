// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.Endo
import cats.effect.IO
import cats.syntax.all._
import clue.TransactionalClient
import clue.data.syntax._
import crystal.react.View
import crystal.react.implicits._
import eu.timepit.refined.auto._
import eu.timepit.refined.types.string._
import explore.AppCtx
import explore.common._
import explore.components.HelpIcon
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.components.undo.UndoButtons
import explore.implicits._
import explore.model.ObsConfiguration
import explore.model.ObsIdSet
import explore.model.ScienceMode
import explore.model.TargetWithId
import explore.model.formats._
import explore.model.util._
import explore.undo.UndoContext
import explore.undo.UndoStacks
import explore.utils._
import japgolly.scalajs.react._
import japgolly.scalajs.react.hooks.Hooks
import japgolly.scalajs.react.util.DefaultEffects.{ Sync => DefaultS }
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.math._
import lucuma.core.model.SourceProfile
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Types._
import lucuma.ui.forms.FormInputEV
import lucuma.ui.implicits._
import lucuma.ui.optics.ChangeAuditor
import lucuma.ui.optics.TruncatedDec
import lucuma.ui.optics.TruncatedRA
import lucuma.ui.optics.ValidFormatInput
import queries.common.TargetQueriesGQL
import queries.schemas.implicits._
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

final case class SiderealTargetEditor(
  uid:           User.Id,
  id:            Target.Id,
  target:        View[Target.Sidereal],
  obsConf:       Option[ObsConfiguration],
  scienceMode:   Option[ScienceMode],
  undoStacks:    View[UndoStacks[IO, Target.Sidereal]],
  searching:     View[Set[Target.Id]],
  obsIdSubset:   Option[ObsIdSet] = None,
  onClone:       TargetWithId => Callback = _ => Callback.empty,
  renderInTitle: Option[Tile.RenderInTitle] = none
) extends ReactFnProps[SiderealTargetEditor](SiderealTargetEditor.component) {
  val baseCoordinates: Coordinates =
    target.zoom(Target.Sidereal.baseCoordinates).get
}

object SiderealTargetEditor {

  type Props = SiderealTargetEditor

  def readonlyView[A](view: View[A]): View[A] = {
    val getA: A => A               = identity
    val noModA: (A => A) => A => A = _ => identity

    view.zoom(getA)(noModA)
  }

  def cloneTarget(targetId: Target.Id, obsIds: ObsIdSet)(implicit
    c:                      TransactionalClient[IO, ObservationDB]
  ): IO[Target.Id] = TargetQueriesGQL.CloneTargetMutation
    .execute[IO](
      CloneTargetInput(targetId = targetId, replaceIn = obsIds.toList.assign)
    )
    .map(_.cloneTarget.id)

  def getRemoteOnMod(
    id:              Target.Id,
    optObs:          Option[ObsIdSet],
    cloning:         Hooks.UseStateF[DefaultS, Boolean],
    onClone:         TargetWithId => Callback
  )(
    input:           EditTargetInput
  )(implicit appCtx: AppContextIO): IO[Unit] =
    optObs.fold(TargetQueriesGQL.EditTargetMutation.execute(input).void) { obsIds =>
      cloning.setState(true).to[IO] >>
        cloneTarget(id, obsIds)
          .flatMap { newId =>
            val newInput = EditTargetInput.select
              .andThen(TargetSelectInput.targetIds)
              .replace(List(newId).assign)(input)
            TargetQueriesGQL.EditTargetMutationWithResult
              .execute(newInput)
              .flatMap(data =>
                (data.editTarget.headOption.foldMap(onClone) >> cloning.setState(false)).to[IO]
              )
          }
    }

  val component =
    ScalaFnComponent
      .withHooks[Props]
      // cloning
      .useState(false)
      .render { (props, cloning) =>
        AppCtx.using { implicit appCtx =>
          // If we're going to clone on edit, use readonly views so we don't update the original
          // target in the model or add the API clone to the undo stack for the original target.
          val (targetView, undoStackView) =
            props.obsIdSubset.fold((props.target, props.undoStacks))(_ =>
              (readonlyView(props.target), readonlyView(props.undoStacks))
            )

          val undoCtx: UndoContext[Target.Sidereal] = UndoContext(undoStackView, targetView)

          val target: Target.Sidereal = props.target.get

          val remoteOnMod: EditTargetInput => IO[Unit] =
            getRemoteOnMod(props.id, props.obsIdSubset, cloning, props.onClone) _

          val siderealTargetAligner: Aligner[Target.Sidereal, EditTargetInput] =
            Aligner(
              undoCtx,
              EditTargetInput(
                select = TargetSelectInput(targetIds = List(props.id).assign),
                patch = TargetPropertiesInput()
              ),
              remoteOnMod
            )

          val nameLens          = EditTargetInput.patch.andThen(TargetPropertiesInput.name)
          val siderealLens      = EditTargetInput.patch.andThen(TargetPropertiesInput.sidereal)
          val sourceProfileLens = EditTargetInput.patch.andThen(TargetPropertiesInput.sourceProfile)

          val allView: View[Target.Sidereal] =
            siderealTargetAligner.viewMod(t =>
              nameLens.replace(t.name.assign) >>>
                siderealLens.replace(t.toInput.assign) >>>
                sourceProfileLens.replace(t.sourceProfile.toInput.assign)
            )

          val siderealToTargetEndo: Endo[SiderealInput] => Endo[EditTargetInput] =
            forceAssign(siderealLens.modify)(SiderealInput())

          val coordsRAView: View[RightAscension] =
            siderealTargetAligner
              .zoom(Target.Sidereal.baseRA, siderealToTargetEndo.compose(SiderealInput.ra.modify))
              .view(_.toInput.assign)

          val coordsDecView: View[Declination] =
            siderealTargetAligner
              .zoom(Target.Sidereal.baseDec, siderealToTargetEndo.compose(SiderealInput.dec.modify))
              .view(_.toInput.assign)

          val epochView: View[Epoch] =
            siderealTargetAligner
              .zoom(Target.Sidereal.epoch, siderealToTargetEndo.compose(SiderealInput.epoch.modify))
              .view((Epoch.fromString.reverseGet _).andThen(_.assign))

          val nameView: View[NonEmptyString] =
            siderealTargetAligner
              .zoom(Target.Sidereal.name, nameLens.modify)
              .view(_.assign)

          val properMotionRAView: View[Option[ProperMotion.RA]] =
            siderealTargetAligner
              .zoom(
                Target.Sidereal.properMotionRA.getOption,
                (f: Endo[Option[ProperMotion.RA]]) =>
                  Target.Sidereal.properMotionRA.modify(unsafeOptionFnUnlift(f)),
                siderealToTargetEndo.compose(SiderealInput.properMotion.modify)
              )
              .view((pmRA: Option[ProperMotion.RA]) =>
                buildProperMotion(pmRA, Target.Sidereal.properMotionDec.getOption(target))
                  .map(_.toInput)
                  .orUnassign
              )

          val properMotionDecView: View[Option[ProperMotion.Dec]] =
            siderealTargetAligner
              .zoom(
                Target.Sidereal.properMotionDec.getOption,
                (f: Endo[Option[ProperMotion.Dec]]) =>
                  Target.Sidereal.properMotionDec.modify(unsafeOptionFnUnlift(f)),
                siderealToTargetEndo.compose(SiderealInput.properMotion.modify)
              )
              .view((pmDec: Option[ProperMotion.Dec]) =>
                buildProperMotion(Target.Sidereal.properMotionRA.getOption(target), pmDec)
                  .map(_.toInput)
                  .orUnassign
              )

          val parallaxView: View[Option[Parallax]] =
            siderealTargetAligner
              .zoom(
                Target.Sidereal.parallax,
                siderealToTargetEndo.compose(SiderealInput.parallax.modify)
              )
              .view(_.map(_.toInput).orUnassign)

          val radialVelocityView: View[Option[RadialVelocity]] =
            siderealTargetAligner
              .zoom(
                Target.Sidereal.radialVelocity,
                siderealToTargetEndo.compose(SiderealInput.radialVelocity.modify)
              )
              .view(_.map(_.toInput).orUnassign)

          val sourceProfileAligner: Aligner[SourceProfile, SourceProfileInput] =
            siderealTargetAligner.zoom(
              Target.Sidereal.sourceProfile,
              forceAssign(sourceProfileLens.modify)(SourceProfileInput())
            )

          def searchAndSet(allView: View[Target.Sidereal], nameView: View[NonEmptyString])(
            s:                      SearchCallback
          ): Callback =
            SimbadSearch
              .search[IO](s.searchTerm)
              .map(_.headOption)
              .runAsyncAndThen {
                case Right(Some(r)) =>
                  allView.set(r.target) >> s.onComplete(r.target.some)
                case Right(None)    =>
                  nameView.set(s.searchTerm) >> s.onComplete(none)
                case Left(t)        =>
                  nameView.set(s.searchTerm) >> s.onError(t)
              }

          val disabled = props.searching.get.exists(_ === props.id) || cloning.value

          React.Fragment(
            props.renderInTitle
              .map(_.apply(<.span(ExploreStyles.TitleUndoButtons)(UndoButtons(undoCtx)))),
            <.div(ExploreStyles.TargetGrid)(
              <.div(
                ExploreStyles.TitleUndoButtons,
                // Don't show the undo/redo buttons if we are in cloning mode or they are in the title bar.
                UndoButtons(undoCtx, disabled = disabled)
                  .when(props.renderInTitle.isEmpty && props.obsIdSubset.isEmpty)
              ),
              AladinCell(
                props.uid,
                props.id,
                props.obsConf,
                props.scienceMode,
                targetView.zoom(Target.Sidereal.tracking)
              ),
              <.div(ExploreStyles.Grid, ExploreStyles.Compact, ExploreStyles.TargetForm)(
                // Keep the search field and the coords always together
                SearchForm(
                  props.id,
                  // SearchForm doesn't edit the name directly. It will set it atomically, together
                  // with coords & magnitudes from the catalog search, so that all 3 fields are
                  // a single undo/redo operation.
                  targetView.zoom(Target.Sidereal.name).get,
                  props.searching,
                  searchAndSet(allView, nameView)
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
              Form(as = <.div, size = Small)(
                ExploreStyles.Grid,
                ExploreStyles.Compact,
                ExploreStyles.ExploreForm,
                ExploreStyles.TargetProperMotionForm,
                <.label("Epoch", HelpIcon("target/main/epoch.md"), ExploreStyles.SkipToNext),
                InputWithUnits(
                  epochView,
                  ValidFormatInput.fromFormat(Epoch.fromStringNoScheme, "Invalid Epoch"),
                  ChangeAuditor.maxLength(8).decimal(3).denyNeg.as[Epoch],
                  id = "epoch",
                  units = "years",
                  disabled = disabled
                ),
                <.label("µ RA", ExploreStyles.SkipToNext),
                InputWithUnits(
                  properMotionRAView,
                  ValidFormatInput.fromFormat(pmRAFormat, "Must be a number").optional,
                  ChangeAuditor.fromFormat(pmRAFormat).decimal(3).optional,
                  id = "raPM",
                  units = "mas/y",
                  disabled = disabled
                ),
                <.label("µ Dec", ExploreStyles.SkipToNext),
                InputWithUnits(
                  properMotionDecView,
                  ValidFormatInput.fromFormat(pmDecFormat, "Must be a number").optional,
                  ChangeAuditor.fromFormat(pmDecFormat).decimal(3).optional,
                  id = "raDec",
                  units = "mas/y",
                  disabled = disabled
                ),
                <.label("Parallax", ExploreStyles.SkipToNext),
                InputWithUnits(
                  parallaxView,
                  ValidFormatInput.fromFormat(pxFormat, "Must be a number").optional,
                  ChangeAuditor.fromFormat(pxFormat).decimal(3).optional,
                  id = "parallax",
                  units = "mas",
                  disabled = disabled
                ),
                RVInput(radialVelocityView, disabled)
              ),
              Form(as = <.div, size = Small)(
                ExploreStyles.Grid,
                ExploreStyles.Compact,
                ExploreStyles.ExploreForm,
                ExploreStyles.TargetSourceProfileEditor,
                ExploreStyles.Gaussian
                  .when(SourceProfile.gaussian.getOption(sourceProfileAligner.get).isDefined)
              )(
                SourceProfileEditor(sourceProfileAligner, disabled = disabled)
              )
            )
          )
        }
      }

}
