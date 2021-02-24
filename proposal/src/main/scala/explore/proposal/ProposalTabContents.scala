// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.proposal

import cats.effect._
import coulomb.accepted.Percent
import coulomb.refined._
import coulomb.time._
import crystal.Ready
import crystal.react.StreamRendererMod
import eu.timepit.refined.auto._
import eu.timepit.refined.numeric._
import explore.AppCtx
import explore.components.WIP
import explore.implicits._
import explore.model._
import explore.model.enum._
import explore.model.refined._
import explore.model.reusability._
import fs2.concurrent.SignallingRef
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model._

import java.net.URI

object ProposalTabContents {
  type Props = View[Option[Focused]]

  // Most of the below will be replaced by a GraphQL subscription
  class Backend() {
    val profileId = OrcidId.fromUri(new URI("https://orcid.org/0000-0001-5148-9668")).toOption.get
    val roleId    = StandardRole.Id(23L)
    val profile   = OrcidProfile(profileId, Some("Edmund"), Some("Stargazer"), None, None)
    val pi        = StandardUser(User.Id(666L), StandardRole.Pi(roleId), Nil, profile)

    val proposalDetails = ProposalDetails(
      "",
      pi,
      ProposalClass.Queue,
      None,
      ToOActivation.None,
      Set.empty,
      "",
      List.empty,
      7.5.withRefinedUnit[NonNegative, Hour],
      24.2.withRefinedUnit[NonNegative, Hour],
      80.withRefinedUnit[ZeroTo100, Percent],
      80.withRefinedUnit[ZeroTo100, Percent]
    )

    def render() =
      AppCtx.withCtx { implicit appCtx =>
        val ref: SignallingRef[IO, ProposalDetails] =
          SignallingRef.in[SyncIO, IO, ProposalDetails](proposalDetails).unsafeRunSync()

        val component = StreamRendererMod.build(ref.discrete)

        component(_ match {
          case Ready(view) => WIP(ProposalDetailsEditor(view))
          case _           => <.div("Ruh-Roh")
        })
      }
  }

  val component = ScalaComponent.builder[Props].renderBackend[Backend].build

  def apply(props: Props) = component(props)
}
