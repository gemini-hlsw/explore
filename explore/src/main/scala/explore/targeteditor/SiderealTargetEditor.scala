// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.Endo
import cats.effect.IO
import cats.syntax.all.*
import clue.data.syntax.*
import crystal.*
import crystal.react.*
import crystal.react.hooks.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.common.*
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.model.AladinFullScreen
import explore.model.AppContext
import explore.model.Asterism
import explore.model.AttachmentList
import explore.model.ExploreModelValidators
import explore.model.GlobalPreferences
import explore.model.GuideStarSelection
import explore.model.ObsConfiguration
import explore.model.ObsIdSet
import explore.model.ObservationsAndTargets
import explore.model.OnCloneParameters
import explore.model.TargetEditObsInfo
import explore.model.reusability.given
import explore.services.OdbAsterismApi
import explore.services.OdbTargetApi
import explore.syntax.ui.*
import explore.undo.UndoSetter
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.math.*
import lucuma.core.math.validation.MathValidators
import lucuma.core.model.Program
import lucuma.core.model.SourceProfile
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.validation.*
import lucuma.react.common.*
import lucuma.react.primereact.Message
import lucuma.refined.*
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.odb.input.*
import lucuma.ui.input.ChangeAuditor
import lucuma.ui.primereact.FormInputTextView
import lucuma.ui.primereact.LucumaPrimeStyles
import lucuma.ui.primereact.given
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import lucuma.utils.*
import org.typelevel.log4cats.Logger

import java.time.Instant

case class SiderealTargetEditor(
  programId:          Program.Id,
  userId:             User.Id,
  target:             UndoSetter[Target.Sidereal],
  obsAndTargets:      UndoSetter[ObservationsAndTargets],
  asterism:           Asterism, // This is passed through to Aladin, to plot the entire Asterism.
  obsTime:            Option[Instant],
  obsConf:            Option[ObsConfiguration],
  searching:          View[Set[Target.Id]],
  obsInfo:            TargetEditObsInfo,
  onClone:            OnCloneParameters => Callback,
  fullScreen:         View[AladinFullScreen],
  globalPreferences:  View[GlobalPreferences],
  guideStarSelection: View[GuideStarSelection],
  attachments:        View[AttachmentList],
  authToken:          Option[NonEmptyString],
  readonly:           Boolean,
  invalidateSequence: Callback = Callback.empty
) extends ReactFnProps(SiderealTargetEditor.component)

object SiderealTargetEditor:
  private type Props = SiderealTargetEditor

  private def cloneTarget(
    programId:     Program.Id,
    targetId:      Target.Id,
    obsIds:        ObsIdSet,
    cloning:       View[Boolean],
    obsAndTargets: UndoSetter[ObservationsAndTargets],
    onClone:       OnCloneParameters => Callback
  )(
    input:         UpdateTargetsInput
  )(using
    odbApi:        OdbTargetApi[IO] & OdbAsterismApi[IO]
  )(using Logger[IO], ToastCtx[IO]): IO[Unit] =
    odbApi
      .cloneTarget(targetId, obsIds, input)
      .flatMap: clone =>
        (TargetCloneAction
          .cloneTarget(programId, targetId, clone, obsIds, onClone)
          .set(obsAndTargets)(clone.target.some) >>
          // If we do the first `onClone` here, the UI works correctly.
          onClone(OnCloneParameters(targetId, clone.id, obsIds, true))).toAsync
      .switching(cloning.async)
      .handleErrorWith: t => // TODO Move error handling to API layer
        val msg = s"Error cloning target [$targetId]"
        Logger[IO].error(t)(msg) >>
          ToastCtx[IO].showToast(msg, Message.Severity.Error)

  private def buildProperMotion(
    ra:  Option[ProperMotion.RA],
    dec: Option[ProperMotion.Dec]
  ): Option[ProperMotion] =
    attemptCombine(ra, dec)
      .map(ProperMotion.apply.tupled)

  // An UndoSetter that doesn't really update any undo stacks
  private def noopUndoSetter[M](view: View[M]): UndoSetter[M] =
    new UndoSetter[M] {
      val model = view
      def set[A](
        getter:    M => A,
        setter:    A => M => M,
        onSet:     (M, A) => IO[Unit],
        onRestore: (M, A) => IO[Unit]
      )(v: A): Callback =
        mod(getter, setter, onSet, onRestore)(_ => v)

      def mod[A](
        getter:    M => A,
        setter:    A => M => M,
        onSet:     (M, A) => IO[Unit],
        onRestore: (M, A) => IO[Unit]
      )(f: A => A): Callback =
        model.modCB(
          oldModel => setter(f(getter(oldModel)))(oldModel),
          (oldModel, newModel) => onSet(oldModel, getter(newModel)).runAsyncAndForget
        )
    }

  private val component =
    ScalaFnComponent[Props]: props =>
      for
        ctx                   <- useContext(AppContext.ctx)
        cloning               <- useStateView(false)
        obsToCloneTo          <- useStateView(none[ObsIdSet]) // obs ids to clone to.
        // If obsTime is not set, change it to now
        obsTime               <- useEffectKeepResultWithDeps(props.obsTime): obsTime =>
                                   IO(obsTime.getOrElse(Instant.now()))
        // select the aligner to use based on whether a clone will be created or not.
        siderealTargetAligner <-
          useMemo(
            (props.programId, props.target.get, props.asterism.focus.id, obsToCloneTo.get)
          ): (pid, target, tid, toCloneTo) =>
            import ctx.given

            toCloneTo.fold(
              Aligner(
                props.target,
                UpdateTargetsInput(
                  WHERE = tid.toWhereTarget.assign,
                  SET = TargetPropertiesInput()
                ),
                // Invalidate the sequence if the target changes
                u => props.invalidateSequence.to[IO] >> ctx.odbApi.updateTarget(tid, u)
              )
            ): obsIds =>
              val view = View(target, (mod, cb) => cb(target, mod(target)))
              Aligner(
                noopUndoSetter(view),
                // noopUndoSetter(noUndoTargetView),
                UpdateTargetsInput(SET = TargetPropertiesInput()),
                u =>
                  props.invalidateSequence.to[IO] *>
                    cloneTarget(
                      pid,
                      tid,
                      obsIds,
                      cloning,
                      props.obsAndTargets,
                      props.onClone
                    )(u)
              )
      yield
        import ctx.given

        val nameLens          = UpdateTargetsInput.SET.andThen(TargetPropertiesInput.name)
        val siderealLens      = UpdateTargetsInput.SET.andThen(TargetPropertiesInput.sidereal)
        val sourceProfileLens =
          UpdateTargetsInput.SET.andThen(TargetPropertiesInput.sourceProfile)

        val allView: View[Target.Sidereal] =
          siderealTargetAligner.viewMod(t =>
            nameLens.replace(t.name.assign) >>>
              siderealLens.replace(t.toInput.assign) >>>
              sourceProfileLens.replace(t.sourceProfile.toInput.assign)
          )

        val siderealToTargetEndo: Endo[SiderealInput] => Endo[UpdateTargetsInput] =
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
            .view(Epoch.fromString.reverseGet.andThen(_.assign))

        val nameView: View[NonEmptyString] =
          siderealTargetAligner
            .zoom(Target.Sidereal.name, nameLens.modify)
            .view(_.assign)

        val properMotionRAView: View[Option[ProperMotion.RA]] =
          siderealTargetAligner
            .zoom(
              Target.Sidereal.properMotionRA.getOption,
              (f: Endo[Option[ProperMotion.RA]]) =>
                Target.Sidereal.properMotion.modify(pmOpt =>
                  buildProperMotion(
                    f(pmOpt.map(ProperMotion.ra.get)),
                    pmOpt.map(ProperMotion.dec.get)
                  )
                ),
              siderealToTargetEndo.compose(SiderealInput.properMotion.modify)
            )
            .view((pmRA: Option[ProperMotion.RA]) =>
              buildProperMotion(
                pmRA,
                Target.Sidereal.properMotionDec.getOption(props.target.get)
              )
                .map(_.toInput)
                .orUnassign
            )

        val properMotionDecView: View[Option[ProperMotion.Dec]] =
          siderealTargetAligner
            .zoom(
              Target.Sidereal.properMotionDec.getOption,
              (f: Endo[Option[ProperMotion.Dec]]) =>
                Target.Sidereal.properMotion.modify(pmOpt =>
                  buildProperMotion(
                    pmOpt.map(ProperMotion.ra.get),
                    f(pmOpt.map(ProperMotion.dec.get))
                  )
                ),
              siderealToTargetEndo.compose(SiderealInput.properMotion.modify)
            )
            .view((pmDec: Option[ProperMotion.Dec]) =>
              buildProperMotion(
                Target.Sidereal.properMotionRA.getOption(props.target.get),
                pmDec
              )
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

        val disabled =
          props.searching.get.exists(
            _ === props.asterism.focus.id
          ) || cloning.get || props.readonly || props.obsInfo.isReadonly

        val oid = props.obsInfo.current.map(_.head)

        React.Fragment(
          TargetCloneSelector(props.obsInfo, obsToCloneTo),
          <.div(ExploreStyles.TargetGrid)(
            obsTime.value.renderPot(ot =>
              AladinCell(
                props.userId,
                oid,
                props.asterism,
                ot,
                props.obsConf,
                props.fullScreen,
                props.globalPreferences,
                props.guideStarSelection
              )
            ),
            <.div(LucumaPrimeStyles.FormColumnVeryCompact, ExploreStyles.TargetForm)(
              // Keep the search field and the coords always together
              SearchForm(
                props.asterism.focus.id,
                // SearchForm doesn't edit the name directly. It will set it atomically, together
                // with coords & magnitudes from the catalog search, so that all 3 fields are
                // a single undo/redo operation.
                nameView,
                allView.set,
                props.searching,
                props.readonly || props.obsInfo.isReadonly,
                cloning.get
              ),
              FormInputTextView(
                id = "ra".refined,
                value = coordsRAView,
                label = React.Fragment("RA", HelpIcon("target/main/coordinates.md".refined)),
                disabled = disabled,
                validFormat = MathValidators.truncatedRA,
                changeAuditor = ChangeAuditor.truncatedRA
              ),
              FormInputTextView(
                id = "dec".refined,
                value = coordsDecView,
                label = React.Fragment("Dec", HelpIcon("target/main/coordinates.md".refined)),
                disabled = disabled,
                validFormat = MathValidators.truncatedDec,
                changeAuditor = ChangeAuditor.truncatedDec
              )
            ),
            <.div(
              LucumaPrimeStyles.FormColumnVeryCompact,
              ExploreStyles.TargetProperMotionForm,
              FormInputTextView(
                id = "epoch".refined,
                value = epochView,
                label = React.Fragment("Epoch", HelpIcon("target/main/epoch.md".refined)),
                disabled = disabled,
                validFormat = MathValidators.epochNoScheme,
                changeAuditor = ChangeAuditor.maxLength(8.refined).decimal(3.refined).denyNeg,
                units = "years"
              ),
              FormInputTextView(
                id = "raPM".refined,
                value = properMotionRAView,
                label = "µ RA",
                disabled = disabled,
                validFormat = ExploreModelValidators.pmRAValidWedge.optional,
                changeAuditor = ChangeAuditor.bigDecimal(3.refined).optional,
                units = "mas/y"
              ),
              FormInputTextView(
                id = "raDec".refined,
                value = properMotionDecView,
                label = "µ Dec",
                disabled = disabled,
                validFormat = ExploreModelValidators.pmDecValidWedge.optional,
                changeAuditor = ChangeAuditor.bigDecimal(3.refined).optional,
                units = "mas/y"
              ),
              FormInputTextView(
                id = "parallax".refined,
                value = parallaxView,
                label = "Parallax",
                disabled = disabled,
                validFormat = ExploreModelValidators.pxValidWedge.optional,
                changeAuditor = ChangeAuditor.bigDecimal(3.refined).optional,
                units = "mas"
              ),
              RVInput(radialVelocityView, disabled, props.obsConf.flatMap(_.calibrationRole))
            ),
            <.div(
              ExploreStyles.Grid,
              ExploreStyles.Compact,
              LucumaPrimeStyles.FormColumnVeryCompact,
              ExploreStyles.TargetSourceProfileEditor,
              ExploreStyles.WithGaussian
                .when(SourceProfile.gaussian.getOption(sourceProfileAligner.get).isDefined),
              ExploreStyles.WithCatalogInfo
                .when(props.target.get.catalogInfo.flatMap(_.objectType).isDefined)
            )(
              // The `withKey` is important because React wasn't updating the BrightnessesEditor
              // or the EmissionsLineEditor when the obsIdSubset changed, resulting in targets always
              // being cloned even when all targets should have been edited.
              SourceProfileEditor(
                props.programId,
                sourceProfileAligner,
                props.target.get.catalogInfo,
                props.attachments,
                props.authToken,
                props.obsConf.flatMap(_.calibrationRole),
                disabled
              ).withKey(obsToCloneTo.get.fold("none")(_.show))
            )
          )
        )
