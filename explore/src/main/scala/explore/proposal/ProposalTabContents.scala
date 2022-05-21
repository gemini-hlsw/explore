// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

// package explore.proposal

// import cats.effect._
// import cats.syntax.all._
// import coulomb.accepted.Percent
// import coulomb.refined._
// import coulomb.time._
// import crystal.Ready
// import crystal.react.implicits._
// import eu.timepit.refined.auto._
// import eu.timepit.refined.numeric._
// import explore.components.WIP
// import explore.implicits._
// import explore.model.refined._
// import explore.model.reusability._
// import fs2.concurrent.SignallingRef
// import japgolly.scalajs.react._
// import japgolly.scalajs.react.vdom.html_<^._
// import lucuma.core.enum
// import react.common.ReactFnProps

// import java.net.URI
// import crystal.react.hooks._
// import explore.model.ProposalDetails
// import lucuma.core.model._

// final case class ProposalTabContents()(implicit val ctx: AppContextIO)
//     extends ReactFnProps[ProposalTabContents](ProposalTabContents.component)

// object ProposalTabContents {
//   type Props = ProposalTabContents

//   // Most of the below will be replaced by a GraphQL subscription
//   final protected case class State(ref: Option[SignallingRef[IO, ProposalDetails]] = none)

//   private val profileId =
//     OrcidId.fromUri(new URI("https://orcid.org/0000-0001-5148-9668")).toOption.get
//   private val roleId    = StandardRole.Id(23L)
//   private val profile   = OrcidProfile(profileId, Some("Edmund"), Some("Stargazer"), None, None)
//   private val pi        = StandardUser(User.Id(666L), StandardRole.Pi(roleId), Nil, profile)

//   private val proposalDetails = ProposalDetails(
//     "",
//     pi,
//     enum.ProposalClass.Queue,
//     None,
//     enum.ToOActivation.None,
//     "",
//     List.empty,
//     7.5.withRefinedUnit[NonNegative, Hour],
//     24.2.withRefinedUnit[NonNegative, Hour],
//     80.withRefinedUnit[ZeroTo100, Percent],
//     80.withRefinedUnit[ZeroTo100, Percent]
//   )

//   val component = ScalaFnComponent
//     .withHooks[Props]
//     .useState(none[SignallingRef[IO, ProposalDetails]])
//     .useEffectOnMountBy((_, state) =>
//       SignallingRef
//         .of[IO, ProposalDetails](proposalDetails)
//         .flatMap(ref => state.setStateAsync(ref.some))
//     )
//     .useStreamViewWithReuseBy((_, state) => state.value.void) { (_, state) => _ =>
//       state.value.fold[fs2.Stream[IO, ProposalDetails]](fs2.Stream.empty)(_.discrete)
//     }
//     .render((_, _, viewPot) =>
//       viewPot match {
//         case Ready(view) => WIP(ProposalDetailsEditor(view))
//         case _           => <.div("Ruh-Roh")
//       }
//     )
// }
