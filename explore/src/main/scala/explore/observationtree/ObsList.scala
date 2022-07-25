// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.effect.IO
import cats.syntax.all._
import crystal.react.View
import crystal.react.hooks._
import crystal.react.implicits._
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.common.ObsQueries
import explore.components.ui.ExploreStyles
import explore.components.undo.UndoButtons
import explore.implicits._
import explore.model.Focused
import explore.model.ObsSummaryWithTitleConstraintsAndConf
import explore.model.enums.AppTab
import explore.model.reusability._
import explore.observationtree.ObsBadge
import explore.undo.KIListMod
import explore.undo.UndoContext
import explore.undo.UndoStacks
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.enums.ObsActiveStatus
import lucuma.core.enums.ObsStatus
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.ui.reusability._
import lucuma.ui.utils._
import react.common.ReactFnProps
import react.common.implicits._
import react.semanticui.elements.button.Button
import react.semanticui.shorthand._
import react.semanticui.sizes._

import ObsQueries._

final case class ObsList(
  observations:     View[ObservationList],
  programId:        Program.Id,
  focusedObs:       Option[Observation.Id],
  focusedTarget:    Option[Target.Id],
  setSummaryPanel:  Callback,
  undoStacks:       View[UndoStacks[IO, ObservationList]]
)(implicit val ctx: AppContextIO)
    extends ReactFnProps[ObsList](ObsList.component) {}

object ObsList {
  type Props = ObsList

  protected val obsListMod =
    KIListMod[ObsSummaryWithTitleConstraintsAndConf, Observation.Id](
      ObsSummaryWithTitleConstraintsAndConf.id
    )

  protected def setObs(programId: Program.Id, obsId: Option[Observation.Id])(implicit
    ctx:                          AppContextIO
  ): Callback =
    ctx.pushPage(AppTab.Observations, programId, obsId.fold(Focused.None)(Focused.singleObs(_)))

  protected def insertObs(
    programId: Program.Id,
    pos:       Int,
    undoCtx:   UndoContext[ObservationList],
    adding:    View[Boolean]
  )(implicit
    ctx:       AppContextIO
  ): IO[Unit] =
    adding.async.set(true) >>
      createObservation[IO](programId)
        .flatMap { obs =>
          ObsListActions
            .obsExistence(obs.id, o => setObs(programId, o.some))
            .mod(undoCtx)(obsListMod.upsert(obs.toTitleAndConstraints, pos))
            .to[IO]
        }
        .guarantee(adding.async.set(false))

  protected val component =
    ScalaFnComponent
      .withHooks[Props]
      // Saved index into the observation list
      .useState(none[Int])
      .useEffectWithDepsBy((props, _) => (props.focusedObs, props.observations.get)) {
        (props, optIndex) => params =>
          implicit val ctx               = props.ctx
          val (focusedObs, observations) = params
          val obsList                    = observations
          focusedObs.fold(optIndex.setState(none)) { obsId =>
            // there is a focused obsId, look for it in the list
            val foundIdx = obsList.getIndex(obsId)
            (optIndex.value, foundIdx) match {
              case (_, Some(fidx))    => optIndex.setState(fidx.some) // focused obs is in list
              case (None, None)       => setObs(props.programId, none) >> optIndex.setState(none)
              case (Some(oidx), None) =>
                // focused obs no longer exists, but we have a previous index.
                val newIdx = math.min(oidx, obsList.length - 1)
                obsList.toList
                  .get(newIdx.toLong)
                  .fold(optIndex.setState(none) >> setObs(props.programId, none))(obsSumm =>
                    optIndex.setState(newIdx.some) >> setObs(props.programId, obsSumm.id.some)
                  )
            }
          }
      }
      // adding new observation
      .useStateView(false)
      .render { (props, _, adding) =>
        implicit val ctx = props.ctx

        val undoCtx      = UndoContext(props.undoStacks, props.observations)
        val observations = props.observations.get.toList

        <.div(ExploreStyles.ObsTreeWrapper)(
          <.div(ExploreStyles.TreeToolbar)(
            Button(
              size = Mini,
              compact = true,
              positive = true,
              icon = Icons.New,
              content = "Obs",
              disabled = adding.get,
              loading = adding.get,
              onClick = insertObs(props.programId, observations.length, undoCtx, adding).runAsync
            ),
            UndoButtons(undoCtx, size = Mini, disabled = adding.get)
          ),
          <.div(
            Button(
              onClick = setObs(props.programId, none) >> props.setSummaryPanel,
              clazz = ExploreStyles.ButtonSummary
            )(
              Icons.ListIcon.clazz(ExploreStyles.PaddedRightIcon),
              "Observations Summary"
            )
          ),
          <.div(ExploreStyles.ObsTree)(
            <.div(ExploreStyles.ObsScrollTree)(
              observations.toTagMod { obs =>
                val focusedObs = obs.id
                val selected   = props.focusedObs.exists(_ === focusedObs)
                <.a(
                  ^.href := ctx.pageUrl(
                    AppTab.Observations,
                    props.programId,
                    Focused.singleObs(focusedObs, props.focusedTarget)
                  ),
                  ExploreStyles.ObsItem |+| ExploreStyles.SelectedObsItem.when_(selected),
                  ^.onClick ==> linkOverride(setObs(props.programId, focusedObs.some))
                )(
                  ObsBadge(
                    obs,
                    selected = selected,
                    setStatusCB = (ObsListActions
                      .obsStatus(obs.id)
                      .set(undoCtx) _).compose((_: ObsStatus).some).some,
                    setActiveStatusCB = (ObsListActions
                      .obsActiveStatus(obs.id)
                      .set(undoCtx) _).compose((_: ObsActiveStatus).some).some,
                    setSubtitleCB = (ObsListActions
                      .obsSubtitle(obs.id)
                      .set(undoCtx) _).compose((_: Option[NonEmptyString]).some).some,
                    deleteCB = ObsListActions
                      .obsExistence(obs.id, o => setObs(props.programId, o.some))
                      .mod(undoCtx)(obsListMod.delete)
                      .some
                  )
                )
              }
            )
          )
        )
      }
}
