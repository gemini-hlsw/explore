// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

// package explore.targeteditor

// import cats.effect.IO
// import crystal.ViewF
// import crystal.react.implicits._
// import crystal.react.reuse._
// import explore.common.TargetQueries.TargetResult
// import explore.common.TargetQueriesGQL._
// import explore.common.UserPreferencesQueries._
// import explore.common.UserPreferencesQueriesGQL._
// import explore.components.Tile
// import explore.components.graphql.LiveQueryRenderMod
// import explore.implicits._
// import explore.model.Constants
// import explore.model.TargetVisualOptions
// import explore.model.reusability._
// import explore.undo._
// import explore.utils._
// import japgolly.scalajs.react._
// import japgolly.scalajs.react.vdom.html_<^._
// import lucuma.core.model.Target
// import lucuma.core.model.User
// import lucuma.schemas.ObservationDB
// import lucuma.ui.reusability._
// import monocle.Focus
// import react.common._

// final case class TargetEditor(
//   uid:              User.Id,
//   tid:              Target.Id,
//   undoStacks:       View[UndoStacks[IO, TargetResult]],
//   searching:        View[Set[Target.Id]],
//   renderInTitle:    Tile.RenderInTitle
// )(implicit val ctx: AppContextIO)
//     extends ReactProps[TargetEditor](TargetEditor.component)

// object TargetEditor {
//   type Props = TargetEditor

//   final case class State(options: TargetVisualOptions)

//   object State {
//     val options  = Focus[State](_.options)
//     val fovAngle = options.andThen(TargetVisualOptions.fovAngle)
//   }

//   protected implicit val propsReuse: Reusability[Props] = Reusability.derive
//   protected implicit val stateReuse: Reusability[State] = Reusability.derive

//   protected class Backend($ : BackendScope[Props, State]) {
//     def render(props: Props) = {
//       implicit val ctx = props.ctx

//       LiveQueryRenderMod[ObservationDB, TargetEditQuery.Data, Option[TargetEditQuery.Data.Target]](
//         TargetEditQuery.query(props.tid).reuseAlways,
//         (TargetEditQuery.Data.target.get _).reuseAlways,
//         List(TargetEditSubscription.subscribe[IO](props.tid)).reuseAlways
//       )(
//         potRender(
//           Reuse
//             .currying(props, ViewF.fromState($))
//             .in(
//               (
//                 props,
//                 state,
//                 targetOpt: View[Option[TargetEditQuery.Data.Target]]
//               ) =>
//                 targetOpt.get.map { _ =>
//                   TargetBody(props.uid,
//                              props.tid,
//                              targetOpt.zoom(_.get)(f => _.map(f)),
//                              props.undoStacks,
//                              props.searching,
//                              state.zoom(State.options),
//                              props.renderInTitle
//                   )
//                 }
//             )
//         )
//       )
//     }
//   }

//   val component =
//     ScalaComponent
//       .builder[Props]
//       .initialState(State(TargetVisualOptions.Default))
//       .renderBackend[Backend]
//       .componentDidMount { $ =>
//         val p            = $.props
//         implicit val ctx = p.ctx
//         UserTargetPreferencesQuery
//           .queryWithDefault[IO](p.uid, p.tid, Constants.InitialFov)
//           .flatMap(v => $.modStateIn[IO](State.fovAngle.replace(v)))
//           .runAsyncAndForgetCB
//       }
//       .configure(Reusability.shouldComponentUpdate)
//       .build
// }
