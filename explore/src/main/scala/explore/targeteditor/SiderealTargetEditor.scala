// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.Endo
import cats.effect.IO
import cats.syntax.all.*
import clue.FetchClient
import clue.data.syntax.*
import crystal.*
import crystal.react.*
import crystal.react.hooks.*
import eu.timepit.refined.types.string.*
import explore.DefaultErrorPolicy
import explore.common.*
import explore.components.HelpIcon
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.model.AladinFullScreen
import explore.model.AppContext
import explore.model.Asterism
import explore.model.ExploreModelValidators
import explore.model.GlobalPreferences
import explore.model.ObsConfiguration
import explore.model.ObsIdSet
import explore.syntax.ui.*
import explore.undo.UndoSetter
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.util.DefaultEffects.Sync as DefaultS
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.math.*
import lucuma.core.math.validation.MathValidators
import lucuma.core.model.SourceProfile
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.validation.*
import lucuma.react.common.*
import lucuma.react.primereact.Message
import lucuma.refined.*
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.model.*
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
import queries.common.TargetQueriesGQL

import java.time.Instant

case class SiderealTargetEditor(
  userId:             User.Id,
  target:             UndoSetter[Target.Sidereal],
  asterism:           Asterism, // This is passed through to Aladin, to plot the entire Asterism.
  vizTime:            Option[Instant],
  obsConf:            Option[ObsConfiguration],
  searching:          View[Set[Target.Id]],
  obsIdSubset:        Option[ObsIdSet] = None,
  onClone:            TargetWithId => Callback = _ => Callback.empty,
  renderInTitle:      Option[Tile.RenderInTitle] = None,
  fullScreen:         View[AladinFullScreen],
  globalPreferences:  View[GlobalPreferences],
  readonly:           Boolean,
  invalidateSequence: Callback = Callback.empty
) extends ReactFnProps(SiderealTargetEditor.component)

object SiderealTargetEditor:
  private type Props = SiderealTargetEditor

  private def cloneTarget(targetId: Target.Id, obsIds: ObsIdSet)(using
    FetchClient[IO, ObservationDB]
  ): IO[Target.Id] = TargetQueriesGQL
    .CloneTargetMutation[IO]
    .execute(
      CloneTargetInput(targetId = targetId, REPLACE_IN = obsIds.toList.assign)
    )
    .map(_.cloneTarget.newTarget.id)

  private def getRemoteOnMod(
    id:      Target.Id,
    optObs:  Option[ObsIdSet],
    cloning: View[Boolean],
    onClone: TargetWithId => Callback
  )(
    input:   UpdateTargetsInput
  )(using FetchClient[IO, ObservationDB], Logger[IO]): IO[Unit] =
    optObs
      .fold(
        TargetQueriesGQL
          .UpdateTargetsMutation[IO]
          .execute(input)
          .void
      ) { obsIds =>
        cloneTarget(id, obsIds)
          .flatMap { newId =>
            val newInput = UpdateTargetsInput.WHERE.replace(newId.toWhereTarget.assign)(input)
            TargetQueriesGQL
              .UpdateTargetsMutationWithResult[IO]
              .execute(newInput)
              .flatMap(data => data.updateTargets.targets.headOption.foldMap(onClone(_).toAsync))
          }
          .switching(cloning.async)

      }

  private def buildProperMotion(
    ra:  Option[ProperMotion.RA],
    dec: Option[ProperMotion.Dec]
  ): Option[ProperMotion] =
    attemptCombine(ra, dec)
      .map(ProperMotion.apply.tupled)

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useStateView(false) // cloning
      // If vizTime is not set, change it to now
      .useEffectResultWithDepsBy((p, _, _) => p.vizTime) { (_, _, _) => vizTime =>
        IO(vizTime.getOrElse(Instant.now()))
      }
      .render { (props, ctx, cloning, vizTime) =>
        import ctx.given

        val remoteOnMod: UpdateTargetsInput => IO[Unit] =
          getRemoteOnMod(
            props.asterism.focus.id,
            props.obsIdSubset,
            cloning,
            props.onClone
          ).andThen(
            _.handleErrorWith(t =>
              Logger[IO].error(t)(s"Error updating target [${props.asterism.focus.id}]") >>
                ToastCtx[IO].showToast(
                  s"Error saving target [${props.asterism.focus.id}]",
                  Message.Severity.Error
                )
            )
          )

        val siderealTargetAligner: Aligner[Target.Sidereal, UpdateTargetsInput] =
          Aligner(
            props.target,
            UpdateTargetsInput(
              WHERE = props.asterism.focus.id.toWhereTarget.assign,
              SET = TargetPropertiesInput()
            ),
            // Invalidate the sequence if the target changes
            u => props.invalidateSequence.to[IO] *> remoteOnMod(u)
          )

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
          props.searching.get.exists(_ === props.asterism.focus.id) || cloning.get || props.readonly

        React.Fragment(
          <.div(ExploreStyles.TargetGrid)(
            vizTime.renderPot(vt =>
              AladinCell(
                props.userId,
                props.asterism,
                vt,
                props.obsConf,
                props.fullScreen,
                props.globalPreferences
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
                props.readonly
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
              RVInput(radialVelocityView, disabled)
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
                sourceProfileAligner,
                props.target.get.catalogInfo,
                disabled
              ).withKey(props.obsIdSubset.mkString)
            )
          )
        )
      }
