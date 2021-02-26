// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.Applicative
import cats.effect.IO
import cats.syntax.all._
import clue.TransactionalClient
import clue.data.syntax._
import crystal.react.implicits._
import explore.AppCtx
import explore.GraphQLSchemas.ObservationDB
import explore.GraphQLSchemas.ObservationDB.Types._
import explore.Icons
import explore.components.InputModal
import explore.observationtree.ObsBadge
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.model.Focused
import explore.model.Focused.FocusedObs
import explore.model.ObsSummary
import explore.model.enum.AppTab
import explore.model.reusability._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.ui.utils._
import react.common.ReactProps
import react.common.implicits._
import react.semanticui.elements.button.Button
import react.semanticui.sizes._
import lucuma.core.model.Observation

final case class ObsList(
  observations: View[List[ObsSummary]],
  focused:      View[Option[Focused]]
) extends ReactProps[ObsList](ObsList.component)

object ObsList {
  type Props = ObsList

  implicit val propsReuse: Reusability[Props] = Reusability.derive

  def createObservation[F[_]: Applicative](
    name:       String
  )(implicit c: TransactionalClient[F, ObservationDB]): F[Unit] =
    ObsQueries.ProgramCreateObservations
      .execute[F](
        CreateObservationInput(programId = "p-2", name = name.assign)
      )
      .void

  def deleteObservation[F[_]: Applicative](id: Observation.Id)(implicit
    c:                                         GraphQLClient[F, ObservationDB]
  ): F[Unit] =
    ObsQueries.ProgramDeleteObservation.execute[F](id).void

  protected val component =
    ScalaComponent
      .builder[Props]
      .render_P { props =>
        AppCtx.withCtx { implicit ctx =>
          <.div(ExploreStyles.ObsTreeWrapper)(
            <.div(ExploreStyles.TreeToolbar)(
              <.div(
                InputModal(
                  "Create new Observation",
                  initialValue = "",
                  label = "Name",
                  placeholder = "Observation name",
                  okLabel = "Create",
                  onComplete = s => createObservation[IO](s).runAsyncAndForgetCB,
                  trigger = Button(size = Mini, compact = true)(
                    Icons.New.size(Small).fitted(true)
                  )
                )
              )
            ),
            <.div(ExploreStyles.ObsTree)(
              <.div(ExploreStyles.ObsScrollTree)(
                props.observations.get.toTagMod { obs =>
                  val focusedObs = FocusedObs(obs.id)
                  val selected   = props.focused.get.exists(_ === focusedObs)
                  <.a(
                    ^.href := ctx.pageUrl(AppTab.Observations, focusedObs.some),
                    ExploreStyles.ObsItem |+| ExploreStyles.SelectedObsItem.when_(selected),
                    ^.onClick ==> linkOverride(props.focused.set(focusedObs.some))
                  )(
                    ObsBadge(obs,
                             ObsBadge.Layout.NameAndConf,
                             selected = selected,
                             deleteObservation[IO](_).runAsyncAndForgetCB
                    )
                  )
                }
              )
            )
          )
        }
      }
      .configure(Reusability.shouldComponentUpdate)
      .build
}
