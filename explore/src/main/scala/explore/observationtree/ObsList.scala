// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.effect.IO
import cats.syntax.all._
import crystal.react.ReuseView
import crystal.react.hooks._
import crystal.react.implicits._
import crystal.react.reuse._
import explore.Icons
import explore.common.ObsQueries
import explore.components.ui.ExploreStyles
import explore.components.undo.UndoButtons
import explore.implicits._
import explore.model.ObsIdSet
import explore.model.ObsSummaryWithTargetsAndConstraints
import explore.model.enum.AppTab
import explore.observationtree.ObsBadge
import explore.undo.KIListMod
import explore.undo.UndoContext
import explore.undo.UndoStacks
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.enum.ObsActiveStatus
import lucuma.core.enum.ObsStatus
import lucuma.core.model.Observation
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
  observations:     ReuseView[ObservationList],
  focusedObs:       Option[Observation.Id],
  focusedTarget:    Option[Target.Id],
  undoStacks:       ReuseView[UndoStacks[IO, ObservationList]]
)(implicit val ctx: AppContextIO)
    extends ReactFnProps[ObsList](ObsList.component) {}

object ObsList {
  type Props = ObsList

  implicit protected val propsReuse: Reusability[Props] = Reusability.derive

  protected val obsListMod =
    KIListMod[ObsSummaryWithTargetsAndConstraints, Observation.Id](
      ObsSummaryWithTargetsAndConstraints.id
    )

  protected def setObs(obsId: Option[Observation.Id])(implicit ctx: AppContextIO): Callback =
    ctx.pushPageSingleObs(AppTab.Observations, obsId, none)

  protected def insertObs(
    pos:     Int,
    undoCtx: UndoContext[ObservationList],
    adding:  ReuseView[Boolean]
  )(implicit
    ctx:     AppContextIO
  ): IO[Unit] =
    adding.async.set(true) >>
      createObservation[IO]()
        .flatMap {
          _.foldMap { obs =>
            ObsListActions
              .obsExistence(obs.id, o => setObs(o.some))
              .mod(undoCtx)(obsListMod.upsert(obs, pos))
              .to[IO]
          }
        }
        .guarantee(adding.async.set(false))

  protected val component =
    ScalaFnComponent
      .withHooks[Props]
      // Saved index into the observation list
      .useState(none[Int])
      .useEffectWithDepsBy((props, _) => (props.focusedObs, props.observations)) {
        (props, optIndex) => params =>
          implicit val ctx               = props.ctx
          val (focusedObs, observations) = params
          val obsList                    = observations.get
          focusedObs.fold(optIndex.setState(none)) { obsId =>
            // there is a focused obsId, look for it in the list
            val foundIdx = obsList.getIndex(obsId)
            (optIndex.value, foundIdx) match {
              case (_, Some(fidx))    => optIndex.setState(fidx.some) // focused obs is in list
              case (None, None)       => setObs(none) >> optIndex.setState(none)
              case (Some(oidx), None) =>
                // focused obs no longer exists, but we have a previous index.
                val newIdx = math.min(oidx, obsList.length - 1)
                obsList.toList
                  .get(newIdx.toLong)
                  .fold(optIndex.setState(none) >> setObs(none))(obsSumm =>
                    optIndex.setState(newIdx.some) >> setObs(obsSumm.id.some)
                  )
            }
          }
      }
      // adding new observation
      .useStateViewWithReuse(false)
      .renderWithReuse { (props, _, adding) =>
        implicit val ctx = props.ctx

        val undoCtx      = UndoContext(props.undoStacks, props.observations)
        val observations = props.observations.get.toList

        <.div(ExploreStyles.ObsTreeWrapper)(
          <.div(ExploreStyles.TreeToolbar)(
            Button(
              size = Mini,
              compact = true,
              icon = Icons.New,
              content = "Obs",
              disabled = adding.get,
              loading = adding.get,
              onClick = insertObs(observations.length, undoCtx, adding).runAsync
            ),
            UndoButtons(undoCtx, size = Mini, disabled = adding.get)
          ),
          <.div(
            Button(onClick = setObs(none), clazz = ExploreStyles.ButtonSummary)(
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
                  ^.href := ctx.pageUrl(AppTab.Observations,
                                        ObsIdSet.one(focusedObs).some,
                                        props.focusedTarget
                  ),
                  ExploreStyles.ObsItem |+| ExploreStyles.SelectedObsItem.when_(selected),
                  ^.onClick ==> linkOverride(setObs(focusedObs.some))
                )(
                  ObsBadge(
                    obs,
                    selected = selected,
                    setStatusCB = (ObsListActions
                      .obsStatus(obs.id)
                      .set(undoCtx) _).compose((_: ObsStatus).some).reuseAlways.some,
                    setActiveStatusCB = (ObsListActions
                      .obsActiveStatus(obs.id)
                      .set(undoCtx) _).compose((_: ObsActiveStatus).some).reuseAlways.some,
                    deleteCB = ObsListActions
                      .obsExistence(obs.id, o => setObs(o.some))
                      .mod(undoCtx)(obsListMod.delete)
                      .reuseAlways
                      .some
                  )
                )
              }
            )
          )
        )
      }
}
