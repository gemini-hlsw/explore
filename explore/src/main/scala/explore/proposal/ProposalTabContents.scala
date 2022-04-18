// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.proposal

import cats.effect._
import cats.syntax.all._
import coulomb.accepted.Percent
import coulomb.refined._
import coulomb.time._
import crystal.Ready
import crystal.react.StreamRendererMod
import crystal.react.implicits._
import crystal.react.reuse._
import eu.timepit.refined.auto._
import eu.timepit.refined.numeric._
import explore.components.WIP
import explore.implicits._
import explore.model._
import explore.model.refined._
import explore.model.reusability._
import fs2.concurrent.SignallingRef
import japgolly.scalajs.react._
import japgolly.scalajs.react.callback.CallbackCatsEffect._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.enum._
import lucuma.core.model._
import react.common.ReactProps

import java.net.URI

case class ProposalTabContents()(implicit val ctx: AppContextIO)
    extends ReactProps[ProposalTabContents](ProposalTabContents.component)

object ProposalTabContents {
  type Props = ProposalTabContents

  // Most of the below will be replaced by a GraphQL subscription
  final protected case class State(ref: Option[SignallingRef[IO, ProposalDetails]] = none)

  private val profileId =
    OrcidId.fromUri(new URI("https://orcid.org/0000-0001-5148-9668")).toOption.get
  private val roleId    = StandardRole.Id(23L)
  private val profile   = OrcidProfile(profileId, Some("Edmund"), Some("Stargazer"), None, None)
  private val pi        = StandardUser(User.Id(666L), StandardRole.Pi(roleId), Nil, profile)

  private val proposalDetails = ProposalDetails(
    "",
    pi,
    ProposalClass.Queue,
    None,
    ToOActivation.None,
    "",
    List.empty,
    7.5.withRefinedUnit[NonNegative, Hour],
    24.2.withRefinedUnit[NonNegative, Hour],
    80.withRefinedUnit[ZeroTo100, Percent],
    80.withRefinedUnit[ZeroTo100, Percent]
  )
  class Backend() {
    def render(props: Props, state: State) =
      state.ref.map { ref =>
        implicit val ctx = props.ctx

        val component = StreamRendererMod.build(ref.discrete)

        component(Reuse.always(_ match {
          case Ready(view) => WIP(ProposalDetailsEditor(view))
          case _           => <.div("Ruh-Roh")
        }))
      }
  }

  val component = ScalaComponent
    .builder[Props]
    .initialState(State())
    .renderBackend[Backend]
    .componentDidMount { $ =>
      implicit val ctx = $.props.ctx
      SignallingRef
        .of[IO, ProposalDetails](proposalDetails)
        .flatMap(ref => $.setStateIn[IO](State(ref.some)))
        .runAsync
    }
    .build
}
