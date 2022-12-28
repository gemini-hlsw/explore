// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.effect.IO
import cats.syntax.all.*
import clue.TransactionalClient
import crystal.react.View
import crystal.react.hooks.*
import crystal.react.implicits.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.components.undo.UndoButtons
import explore.model.AppContext
import explore.model.Focused
import explore.model.ObsSummaryWithTitleConstraintsAndConf
import explore.model.enums.AppTab
import explore.model.reusability.given
import explore.observationtree.ObsBadge
import explore.undo.KIListMod
import explore.undo.UndoContext
import explore.undo.UndoStacks
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.ObsActiveStatus
import lucuma.core.enums.ObsStatus
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.schemas.ObservationDB
import lucuma.ui.primereact.*
import lucuma.ui.reusability.*
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import lucuma.ui.utils.*
import org.typelevel.log4cats.Logger
import queries.schemas.odb.ObsQueries
import react.common.ReactFnProps
import react.primereact.Button

import ObsQueries.*

case class ObsList(
  observations:    View[ObservationList],
  programId:       Program.Id,
  focusedObs:      Option[Observation.Id],
  focusedTarget:   Option[Target.Id],
  setSummaryPanel: Callback,
  undoStacks:      View[UndoStacks[IO, ObservationList]]
) extends ReactFnProps(ObsList.component) {}

object ObsList:
  private type Props = ObsList

  private val obsListMod =
    KIListMod[ObsSummaryWithTitleConstraintsAndConf, Observation.Id](
      ObsSummaryWithTitleConstraintsAndConf.id
    )

  private def setObs(
    programId: Program.Id,
    obsId:     Option[Observation.Id],
    ctx:       AppContext[IO]
  ): Callback =
    ctx.pushPage(AppTab.Observations, programId, obsId.fold(Focused.None)(Focused.singleObs(_)))

  private def insertObs(
    programId: Program.Id,
    pos:       Int,
    undoCtx:   UndoContext[ObservationList],
    adding:    View[Boolean],
    ctx:       AppContext[IO]
  ): IO[Unit] =
    import ctx.given

    adding.async.set(true) >>
      createObservation[IO](programId)
        .flatMap { obs =>
          ObsListActions
            .obsExistence(obs.id, o => setObs(programId, o.some, ctx))
            .mod(undoCtx)(obsListMod.upsert(obs.toTitleAndConstraints, pos))
            .to[IO]
        }
        .guarantee(adding.async.set(false))

  private def cloneObs(
    programId: Program.Id,
    obsId:     Observation.Id,
    pos:       Int,
    undoCtx:   UndoContext[ObservationList],
    adding:    View[Boolean],
    ctx:       AppContext[IO]
  ): IO[Unit] =
    import ctx.given

    adding.async.set(true) >>
      cloneObservation[IO](obsId)
        .flatMap { obs =>
          ObsListActions
            .obsExistence(obs.id, o => setObs(programId, o.some, ctx))
            .mod(undoCtx)(obsListMod.upsert(obs.toTitleAndConstraints, pos))
            .to[IO]
        }
        .guarantee(adding.async.set(false))

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      // Saved index into the observation list
      .useState(none[Int])
      .useEffectWithDepsBy((props, _, _) => (props.focusedObs, props.observations.get)) {
        (props, ctx, optIndex) => params =>
          import ctx.given

          val (focusedObs, obsList) = params

          focusedObs.fold(optIndex.setState(none)) { obsId =>
            // there is a focused obsId, look for it in the list
            val foundIdx = obsList.getIndex(obsId)
            (optIndex.value, foundIdx) match {
              case (_, Some(fidx))    =>
                optIndex.setState(fidx.some) // focused obs is in list
              case (None, None)       =>
                setObs(props.programId, none, ctx) >> optIndex.setState(none)
              case (Some(oidx), None) =>
                // focused obs no longer exists, but we have a previous index.
                val newIdx = math.min(oidx, obsList.length - 1)
                obsList.toList
                  .get(newIdx.toLong)
                  .fold(optIndex.setState(none) >> setObs(props.programId, none, ctx))(obsSumm =>
                    optIndex.setState(newIdx.some) >> setObs(props.programId, obsSumm.id.some, ctx)
                  )
            }
          }
      }
      // adding new observation
      .useStateView(false)
      .render { (props, ctx, _, adding) =>
        import ctx.given

        val undoCtx      = UndoContext(props.undoStacks, props.observations)
        val observations = props.observations.get.toList

        <.div(ExploreStyles.ObsTreeWrapper)(
          <.div(ExploreStyles.TreeToolbar)(
            Button(
              severity = Button.Severity.Success,
              icon = Icons.New,
              label = "Obs",
              disabled = adding.get,
              loading = adding.get,
              onClick =
                insertObs(props.programId, observations.length, undoCtx, adding, ctx).runAsync
            ).mini.compact,
            UndoButtons(undoCtx, size = PlSize.Mini, disabled = adding.get)
          ),
          <.div(
            Button(
              severity = Button.Severity.Secondary,
              icon = Icons.ListIcon,
              label = "Observations Summary",
              onClick = setObs(props.programId, none, ctx) >> props.setSummaryPanel,
              clazz = ExploreStyles.ButtonSummary
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
                  ^.onClick ==> linkOverride(setObs(props.programId, focusedObs.some, ctx))
                )(
                  ObsBadge(
                    obs,
                    selected = selected,
                    setStatusCB = (ObsListActions
                      .obsEditStatus(obs.id)
                      .set(undoCtx) _).compose((_: ObsStatus).some).some,
                    setActiveStatusCB = (ObsListActions
                      .obsActiveStatus(obs.id)
                      .set(undoCtx) _).compose((_: ObsActiveStatus).some).some,
                    setSubtitleCB = (ObsListActions
                      .obsEditSubtitle(obs.id)
                      .set(undoCtx) _).compose((_: Option[NonEmptyString]).some).some,
                    deleteCB = ObsListActions
                      .obsExistence(obs.id, o => setObs(props.programId, o.some, ctx))
                      .mod(undoCtx)(obsListMod.delete)
                      .some,
                    cloneCB = cloneObs(
                      props.programId,
                      obs.id,
                      observations.length,
                      undoCtx,
                      adding,
                      ctx
                    ).runAsync.some
                  )
                )
              }
            )
          )
        )
      }
