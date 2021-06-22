// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.Applicative
import cats.ApplicativeError
import cats.effect.IO
import cats.effect.SyncIO
import cats.syntax.all._
import clue.TransactionalClient
import clue.data.syntax._
import crystal.react.implicits._
import crystal.react.reuse._
import eu.timepit.refined.types.numeric.PosLong
import explore.AppCtx
import explore.Icons
import explore.common.ObsQueries
import explore.common.ObsQueriesGQL._
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
import explore.optics.GetAdjust
import explore.schemas.ObservationDB
import explore.schemas.ObservationDB.Types._
import explore.undo.Action
import explore.undo.KIListMod
import explore.undo.UndoContext
import explore.undo.UndoStacks
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.enum.ObsStatus
import lucuma.core.model.Observation
import lucuma.ui.utils._
import monocle.function.Field1.first
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
    new KIListMod[ObsSummaryWithPointingAndConstraints, Observation.Id](
      ObsSummaryWithPointingAndConstraints.id
    )

  protected class Backend($ : BackendScope[Props, Unit]) {
    def createObservation[F[_]](obsId: Observation.Id)(implicit
      F:                               ApplicativeError[F, Throwable],
      c:                               TransactionalClient[F, ObservationDB]
    ): F[Unit] =
      ProgramCreateObservation
        .execute[F](
          CreateObservationInput(programId = "p-2", observationId = obsId.assign)
        )
        .void

    def deleteObservation[F[_]: Applicative](id: Observation.Id)(implicit
      c:                                         TransactionalClient[F, ObservationDB]
    ): F[Unit] =
      ProgramDeleteObservation.execute[F](id).void

    def undeleteObservation[F[_]: Applicative](id: Observation.Id)(implicit
      c:                                           TransactionalClient[F, ObservationDB]
    ): F[Unit] =
      ProgramUndeleteObservation.execute[F](id).void

    protected def obsStatus[F[_]: Applicative](obsId: Observation.Id)(implicit
      c:                                              TransactionalClient[F, ObservationDB]
    ) = Action[F](
      obsListMod
        .withKey(obsId)
        .composeOptionLens(first)
        .composeOptionLens(ObsSummaryWithPointingAndConstraints.status)
    )((_, status) =>
      UpdateObservationMutation
        .execute[F](
          EditObservationInput(observationId = obsId, status = status.orIgnore)
        )
        .void
    )

    private def obsMod(
      undoCtx: UndoCtx[ObservationList],
      focused: View[Option[Focused]],
      obsId:   Observation.Id
    )(implicit
      c:       TransactionalClient[IO, ObservationDB]
    ): obsListMod.Operation => SyncIO[Unit] = {
      val obsWithId: GetAdjust[ObservationList, obsListMod.ElemWithIndexOpt] =
        obsListMod.withKey(obsId)

      undoCtx
        .mod[obsListMod.ElemWithIndexOpt](
          obsWithId.getter.get,
          obsWithId.adjuster.set,
          onSet = (_: obsListMod.ElemWithIndexOpt).fold {
            deleteObservation[IO](obsId)
          } { case (obs, _) =>
            createObservation[IO](obs.id) >>
              focused.set(Focused.FocusedObs(obs.id).some).to[IO]
          },
          onRestore = (_: obsListMod.ElemWithIndexOpt).fold {
            deleteObservation[IO](obsId)
          } { case (obs, _) =>
            undeleteObservation[IO](obs.id) >>
              focused.set(Focused.FocusedObs(obs.id).some).to[IO]
          }
        )
    }

    protected def newObs(undoCtx: UndoCtx[ObservationList])(implicit
      c:                          TransactionalClient[IO, ObservationDB]
    ): IO[Unit] = {
      // Temporary measure until we have id pools.
      val newObs = IO(Random.nextInt(0xfff)).map(int =>
        ObsSummaryWithPointingAndConstraints(Observation.Id(PosLong.unsafeFrom(int.abs.toLong + 1)),
                                             pointing = none,
                                             constraints = ConstraintsSummary.default,
                                             ObsStatus.New,
                                             Duration.ZERO
        )
      )

      $.propsIn[IO] >>= { props =>
        newObs >>= { obs =>
          val mod = obsMod(undoCtx, props.focused, obs.id)
          mod(obsListMod.upsert(obs, props.observations.get.length)).to[IO]
        }
      }
    }

    def render(props: Props): VdomNode =
      AppCtx.using { implicit ctx =>
        val undoCtx      = UndoContext(props.undoStacks, props.observations)
        val observations = props.observations.get.toList

        def deleteObs(obsId: Observation.Id): SyncIO[Unit] = {
          val mod = obsMod(undoCtx, props.focused, obsId)
          mod(obsListMod.delete)
        }

        <.div(ExploreStyles.ObsTreeWrapper)(
          <.div(ExploreStyles.TreeToolbar)(
            Button(size = Mini,
                   compact = true,
                   icon = Icons.New,
                   content = "Obs",
                   onClick = newObs(undoCtx).runAsyncCB
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
                    setStatusCB = (obsStatus[IO](obs.id)
                      .set(undoCtx) _).compose((_: ObsStatus).some).reuseAlways.some,
                    deleteCB = (deleteObs _).reuseAlways.some
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
          case other                                          => other.some
        })
      }
      .configure(Reusability.shouldComponentUpdate)
      .build
}
