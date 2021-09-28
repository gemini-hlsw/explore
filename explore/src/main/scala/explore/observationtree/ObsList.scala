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
import explore.model.Focused
import explore.model.Focused.FocusedObs
import explore.model.ObsSummaryWithPointingAndConstraints
import explore.model.enum.AppTab
import explore.model.reusability._
import explore.observationtree.ObsBadge
import explore.schemas.ObservationDB
import explore.undo.KIListMod
import explore.undo.UndoContext
import explore.undo.UndoStacks
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.enum.ObsActiveStatus
import lucuma.core.enum.ObsStatus
import lucuma.core.model.Observation
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
  focused:          View[Option[Focused]],
  undoStacks:       View[UndoStacks[IO, ObservationList]]
)(implicit val ctx: AppContextIO)
    extends ReactProps[ObsList](ObsList.component)

object ObsList {
  type Props = ObsList

  implicit protected val propsReuse: Reusability[Props] = Reusability.derive

  protected val obsListMod =
    KIListMod[ObsSummaryWithPointingAndConstraints, Observation.Id](
      ObsSummaryWithPointingAndConstraints.id
    )

  protected class Backend {
    protected def insertObs(
      pos:     Int,
      focused: View[Option[Focused]],
      undoCtx: UndoCtx[ObservationList]
    )(implicit
      c: TransactionalClient[IO, ObservationDB]
    ): SyncIO[Unit] = {
      // Temporary measure until we have id pools.
      val newObs = SyncIO(Random.nextInt(0xfff)).map(int =>
        ObsSummaryWithPointingAndConstraints(
          Observation.Id(PosLong.unsafeFrom(int.abs.toLong + 1)),
          pointing = none,
          constraints = ConstraintsSummary.default,
          ObsStatus.New,
          ObsActiveStatus.Active,
          Duration.ZERO
        )
      )

      newObs.flatMap { obs =>
        ObsListActions
          .obsExistence[IO](obs.id, focused)
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
                   onClick = insertObs(observations.length, props.focused, undoCtx)
            ),
            UndoButtons(undoCtx, size = Mini)
          ),
          <.div(ExploreStyles.ObsTree)(
            <.div(ExploreStyles.ObsScrollTree)(
              <.div(
                Button(onClick = props.focused.set(none), clazz = ExploreStyles.ButtonSummary)(
                  Icons.ListIcon.clazz(ExploreStyles.PaddedRightIcon),
                  "Observations Summary"
                )
              ),
              observations.toTagMod { obs =>
                val focusedObs = FocusedObs(obs.id)
                val selected   = props.focused.get.exists(_ === focusedObs)
                <.a(
                  ^.href := ctx.pageUrl(AppTab.Observations, focusedObs.some),
                  ExploreStyles.ObsItem |+| ExploreStyles.SelectedObsItem.when_(selected),
                  ^.onClick ==> linkOverride(
                    props.focused.set(focusedObs.some)
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
                      .obsExistence[IO](obs.id, props.focused)
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
        $.props.focused.mod(_.flatMap {
          case FocusedObs(oid) if !observations.contains(oid) => none
          case other                                          => other.some
        })
      }
      .componentDidUpdate { $ =>
        val prevObservations = $.prevProps.observations.get
        val observations     = $.currentProps.observations.get

        // If focused observation does not exist anymore, then focus on closest one.
        $.currentProps.focused.mod(_.flatMap {
          case FocusedObs(oid) if !observations.contains(oid) =>
            prevObservations
              .getIndex(oid)
              .flatMap { idx =>
                observations.toList.get(math.min(idx, observations.length - 1).toLong)
              }
              .map(newObs => FocusedObs(newObs.id))
          case other => other.some
        })
      }
      .configure(Reusability.shouldComponentUpdate)
      .build
}
