// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.Applicative
import cats.ApplicativeError
import cats.effect.IO
import cats.syntax.all._
import clue.TransactionalClient
import clue.data.syntax._
import crystal.react.implicits._
import eu.timepit.refined.types.numeric.PosLong
import explore.AppCtx
import explore.Icons
import explore.common.ObsQueries
import explore.common.ObsQueriesGQL._
import explore.components.ui.ExploreStyles
import explore.components.undo.UndoButtons
import explore.components.undo.UndoRegion
import explore.implicits._
import explore.model.Focused
import explore.model.Focused.FocusedObs
import explore.model.ObsSummaryWithPointingAndConstraints
import explore.model.enum.AppTab
import explore.model.reusability._
import explore.observationtree.ObsBadge
import explore.optics.Adjuster
import explore.optics.GetAdjust
import explore.schemas.ObservationDB
import explore.schemas.ObservationDB.Types._
import explore.undo.KIListMod
import explore.undo.Undoer
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Observation
import lucuma.ui.utils._
import react.common.ReactProps
import react.common.implicits._
import react.semanticui.elements.button.Button
import react.semanticui.sizes._

import scala.util.Random

import ObsQueries._

final case class ObsList(
  observations:     View[ObservationList],
  focused:          View[Option[Focused]]
)(implicit val ctx: AppContextIO)
    extends ReactProps[ObsList](ObsList.component)

object ObsList {
  type Props = ObsList

  implicit val propsReuse: Reusability[Props] = Reusability.derive

  val obsListMod =
    new KIListMod[IO, ObsSummaryWithPointingAndConstraints, Observation.Id](
      ObsSummaryWithPointingAndConstraints.id
    )

  class Backend($ : BackendScope[Props, Unit]) {

    def createObservation[F[_]](obsId: Observation.Id)(implicit
      F:                               ApplicativeError[F, Throwable],
      c:                               TransactionalClient[F, ObservationDB]
    ): F[Unit] =
      ProgramCreateObservation
        .execute[F](
          CreateObservationInput(programId = "p-2",
                                 observationId = obsId.assign
                                 //  name = name.orIgnore
          )
        )
        .void
        .handleErrorWith { _ =>
          ProgramUndeleteObservation.execute(obsId).void
        }

    def deleteObservation[F[_]: Applicative](id: Observation.Id)(implicit
      c:                                         TransactionalClient[F, ObservationDB]
    ): F[Unit] =
      ProgramDeleteObservation.execute[F](id).void

    private def setObsWithIndex(
      observations:       View[ObservationList],
      focused:            View[Option[Focused]],
      obsId:              Observation.Id,
      obsWithIndexSetter: Adjuster[ObservationList, obsListMod.ElemWithIndex],
      nextToFocus:        Option[ObsSummaryWithPointingAndConstraints]
    )(implicit
      c:                  TransactionalClient[IO, ObservationDB]
    ): obsListMod.ElemWithIndex => IO[Unit] =
      obsWithIndex =>
        // 1) Update internal model
        observations
          .mod(obsWithIndexSetter.set(obsWithIndex)) >>
          // 2) Send mutation & adjust focus
          obsWithIndex.fold(
            focused.set(nextToFocus.map(f => Focused.FocusedObs(f.id))) >>
              deleteObservation[IO](obsId)
          ) { case (obs, _) =>
            createObservation[IO](obs.id) >>
              focused.set(Focused.FocusedObs(obs.id).some)
          }

    private def obsMod(
      setter:        Undoer.Setter[IO, ObservationList],
      observations:  View[ObservationList],
      focused:       View[Option[Focused]],
      obsId:         Observation.Id,
      focusOnDelete: Option[ObsSummaryWithPointingAndConstraints]
    )(implicit
      c:             TransactionalClient[IO, ObservationDB]
    ): obsListMod.Operation => IO[Unit] = {
      val obsWithId: GetAdjust[ObservationList, obsListMod.ElemWithIndex] =
        obsListMod.withKey(obsId)

      setter
        .mod[obsListMod.ElemWithIndex](
          observations.get,
          obsWithId.getter.get,
          setObsWithIndex(observations, focused, obsId, obsWithId.adjuster, focusOnDelete)
        )
    }

    protected def newObs(setter: Undoer.Setter[IO, ObservationList])(implicit
      c:                         TransactionalClient[IO, ObservationDB]
    ): IO[Unit] = {
      // Temporary measure until we have id pools.
      val newObs = IO(Random.nextInt(0xfff)).map(int =>
        ObsSummaryWithPointingAndConstraints(Observation.Id(PosLong.unsafeFrom(int.abs.toLong + 1)),
                                             pointing = none,
                                             constraints = none
        )
      )

      $.propsIn[IO] >>= { props =>
        newObs >>= { obs =>
          val mod = obsMod(setter, props.observations, props.focused, obs.id, none)
          mod(obsListMod.upsert(obs, props.observations.get.length))
        }
      }
    }

    private def renderFn(props: Props, undoCtx: Undoer.Context[IO, ObservationList]): VdomNode =
      AppCtx.using { implicit ctx =>
        val focused       = props.focused.get
        val observations  = props.observations.get.toList
        val someSelected  = focused.isDefined
        val obsWithIdx    = observations.zipWithIndex
        val obsId         = focused.collect { case FocusedObs(id) => id }
        val obsIdx        = obsWithIdx.find(i => obsId.forall(_ === i._1.id)).foldMap(_._2)
        val nextToSelect  = obsWithIdx.find(_._2 === obsIdx + 1).map(_._1)
        val prevToSelect  = obsWithIdx.find(_._2 === obsIdx - 1).map(_._1)
        val focusOnDelete = nextToSelect.orElse(prevToSelect).filter(_ => someSelected)

        def deleteObs(obsId: Observation.Id): IO[Unit] = {
          val mod = obsMod(undoCtx.setter, props.observations, props.focused, obsId, focusOnDelete)
          mod(obsListMod.delete)
        }

        <.div(ExploreStyles.ObsTreeWrapper)(
          <.div(ExploreStyles.TreeToolbar)(
            Button(size = Mini,
                   compact = true,
                   icon = Icons.New,
                   content = "Obs",
                   onClick = newObs(undoCtx.setter).runAsyncCB
            ),
            UndoButtons(props.observations.get, undoCtx, size = Mini)
          ),
          <.div(ExploreStyles.ObsTree)(
            <.div(ExploreStyles.ObsScrollTree)(
              <.div(
                Button(onClick = props.focused.set(none).runAsyncCB,
                       clazz = ExploreStyles.ButtonSummary
                )(
                  Icons.List,
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
                    deleteCB = (deleteObs _).reusable.some
                  )
                )
              }
            )
          )
        )
      }

    def render(props: Props) =
      UndoRegion[ObservationList]((renderFn _).reusable(props))
  }

  protected val component =
    ScalaComponent
      .builder[Props]
      .renderBackend[Backend]
      .componentDidMount { $ =>
        implicit val ctx = $.props.ctx
        val observations = $.props.observations.get

        // Unfocus if focused element is not in list.
        val unfocus =
          $.props.focused.get.map { focused =>
            $.props.focused
              .set(none)
              .whenA(focused match {
                case FocusedObs(oid) => !observations.contains(oid)
                case _               => true // If focused on something else, unfocus too.
              })
          }.orEmpty

        unfocus.runAsyncAndForgetCB
      }
      .configure(Reusability.shouldComponentUpdate)
      .build
}
