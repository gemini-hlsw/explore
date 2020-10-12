// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.proposal

import java.net.URI

import cats.effect._
import crystal.Ready
import crystal.react.StreamRendererMod
import eu.timepit.refined.auto._
import explore.AppCtx
import explore.implicits._
import explore.model._
import explore.model.enum._
import explore.model.reusability._
import fs2.concurrent.SignallingRef
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model._

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
      ProposalClass.Exchange,
      TacCategory.StarAndPlanetFormation,
      ToOActivation.None,
      Set.empty,
      "",
      List.empty,
      24.2,
      7.5
    )

    def render() =
      AppCtx.withCtx { implicit appCtx =>
        val ref: SignallingRef[IO, ProposalDetails] =
          SignallingRef.in[SyncIO, IO, ProposalDetails](proposalDetails).unsafeRunSync()

        val component = StreamRendererMod.build(ref.discrete)

        component(_ match {
          case Ready(view) => ProposalDetailsEditor(view)
          case _           => <.div("Ruh-Roh")
        })
      }
  }

  val component = ScalaComponent.builder[Props].renderBackend[Backend].build

  def apply(props: Props) = component(props)
}
