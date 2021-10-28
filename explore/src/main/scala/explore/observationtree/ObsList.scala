// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.effect.IO
import cats.effect.SyncIO
import cats.syntax.all._
import clue.TransactionalClient
import crystal.react.implicits._
import crystal.react.reuse._
import eu.timepit.refined.types.numeric.PosLong
import explore.AppCtx
import explore.Icons
import explore.common.ObsQueries
import explore.components.ui.ExploreStyles
import explore.components.undo.UndoButtons
import explore.implicits._
import explore.model.ConstraintsSummary
import explore.model.FocusedObs
import explore.model.ObsSummaryWithTargetsAndConstraints
import explore.model.enum.AppTab
import explore.model.reusability._
import explore.observationtree.ObsBadge
import explore.undo.KIListMod
import explore.undo.UndoContext
import explore.undo.UndoStacks
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.enum.ObsActiveStatus
import lucuma.core.enum.ObsStatus
import lucuma.core.model.Observation
import lucuma.schemas.ObservationDB
import lucuma.ui.utils._
import react.common.ReactProps
import react.common.implicits._
import react.semanticui.elements.button.Button
import react.semanticui.shorthand._
import react.semanticui.sizes._

import java.time.Duration
import scala.util.Random

import ObsQueries._

final case class ObsList(
  observations:     View[ObservationList],
  focusedObs:       View[Option[FocusedObs]],
  undoStacks:       View[UndoStacks[IO, ObservationList]]
)(implicit val ctx: AppContextIO)
    extends ReactProps[ObsList](ObsList.component)

object ObsList {
  type Props = ObsList

  implicit protected val propsReuse: Reusability[Props] = Reusability.derive

  protected val obsListMod =
    KIListMod[ObsSummaryWithTargetsAndConstraints, Observation.Id](
      ObsSummaryWithTargetsAndConstraints.id
    )

  protected class Backend {
    protected def insertObs(
      pos:        Int,
      focusedObs: View[Option[FocusedObs]],
      undoCtx:    UndoCtx[ObservationList]
    )(implicit
      c:          TransactionalClient[IO, ObservationDB]
    ): SyncIO[Unit] = {
      // Temporary measure until we have id pools.
      val newObs = SyncIO(Random.nextInt(0xfff)).map(int =>
        ObsSummaryWithTargetsAndConstraints(
          Observation.Id(PosLong.unsafeFrom(int.abs.toLong + 1)),
          targets = List.empty,
          constraints = ConstraintsSummary.default,
          ObsStatus.New,
          ObsActiveStatus.Active,
          Duration.ZERO
        )
      )

      newObs.flatMap { obs =>
        ObsListActions
          .obsExistence[IO](obs.id, focusedObs)
          .mod(undoCtx)(obsListMod.upsert(obs, pos))
      }
    }

    def render(props: Props): VdomNode =
      AppCtx.using { implicit ctx =>
        val undoCtx      = UndoContext(props.undoStacks, props.observations)
        val observations = props.observations.get.toList

        <.div(ExploreStyles.ObsTreeWrapper)(
          <.div(ExploreStyles.TreeToolbar)(
            Button(size = Mini,
                   compact = true,
                   icon = Icons.New,
                   content = "Obs",
                   onClick = insertObs(observations.length, props.focusedObs, undoCtx)
            ),
            UndoButtons(undoCtx, size = Mini)
          ),
          <.div(ExploreStyles.ObsTree)(
            <.div(ExploreStyles.ObsScrollTree)(
              <.div(
                Button(onClick = props.focusedObs.set(none), clazz = ExploreStyles.ButtonSummary)(
                  Icons.ListIcon.clazz(ExploreStyles.PaddedRightIcon),
                  "Observations Summary"
                )
              ),
              observations.toTagMod { obs =>
                val focusedObs = FocusedObs(obs.id)
                val selected   = props.focusedObs.get.exists(_ === focusedObs)
                <.a(
                  ^.href := ctx.pageUrl(AppTab.Observations, focusedObs.some),
                  ExploreStyles.ObsItem |+| ExploreStyles.SelectedObsItem.when_(selected),
                  ^.onClick ==> linkOverride(
                    props.focusedObs.set(focusedObs.some)
                  )
                )(
                  ObsBadge(
                    obs,
                    selected = selected,
                    setStatusCB = (ObsListActions
                      .obsStatus[IO](obs.id)
                      .set(undoCtx) _).compose((_: ObsStatus).some).reuseAlways.some,
                    setActiveStatusCB = (ObsListActions
                      .obsActiveStatus[IO](obs.id)
                      .set(undoCtx) _).compose((_: ObsActiveStatus).some).reuseAlways.some,
                    deleteCB = ObsListActions
                      .obsExistence[IO](obs.id, props.focusedObs)
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

  protected val component =
    ScalaComponent
      .builder[Props]
      .renderBackend[Backend]
      .componentDidMount { $ =>
        val observations = $.props.observations.get

        // If focused observation does not exist anymore, then unfocus.
        $.props.focusedObs.mod(_.flatMap {
          case FocusedObs(oid) if !observations.contains(oid) => none
          case other                                          => other.some
        })
      }
      .componentDidUpdate { $ =>
        val prevObservations = $.prevProps.observations.get
        val observations     = $.currentProps.observations.get

        // If focused observation does not exist anymore, then focus on closest one.
        $.currentProps.focusedObs.mod(_.flatMap {
          case FocusedObs(oid) if !observations.contains(oid) =>
            prevObservations
              .getIndex(oid)
              .flatMap { idx =>
                observations.toList.get(math.min(idx, observations.length - 1).toLong)
              }
              .map(newObs => FocusedObs(newObs.id))
          case other                                          => other.some
        })
      }
      .configure(Reusability.shouldComponentUpdate)
      .build
}
