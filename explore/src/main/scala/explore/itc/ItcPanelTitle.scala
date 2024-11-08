// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

// package explore.itc

// import cats.syntax.all.*
// import crystal.*
// import crystal.react.*
// import crystal.react.hooks.*
// import eu.timepit.refined.*
// import explore.Icons
// import explore.components.ui.ExploreStyles
// import explore.model.Constants.MissingInfoMsg
// import explore.model.itc.ItcAsterismGraphResults
// import explore.model.itc.ItcGraphResult
// import explore.model.itc.ItcTarget
// import explore.model.reusability.given
// import japgolly.scalajs.react.*
// import japgolly.scalajs.react.vdom.html_<^.*
// import lucuma.react.common.ReactFnProps
// import lucuma.react.floatingui.syntax.*
// import lucuma.react.primereact.Dropdown
// import lucuma.react.primereact.SelectItem
// import lucuma.ui.syntax.all.given
// import lucuma.ui.syntax.pot.*
// import lucuma.ui.utils.*

// case class ItcPanelTitle(
//   itcPanelProps:   ItcProps,
//   itcGraphResults: Pot[ItcAsterismGraphResults],
//   tileState:       View[SelectedItcTarget]
// ) extends ReactFnProps(ItcPanelTitle.component):
//   val selectedTarget = tileState.zoom(SelectedItcTarget.value.asLens)

// object ItcPanelTitle:
//   private type Props = ItcPanelTitle

//   private val component =
//     ScalaFnComponent
//       .withHooks[Props]
//       // Reset the selected target if it changes
//       .useEffectWhenDepsReadyOrChangeBy(props => props.itcGraphResults.map(_.brightestTarget)):
//         props => itcBrightestTarget => props.selectedTarget.set(itcBrightestTarget)
//       .render: props =>
//         def newSelected(p: Int): Option[ItcTarget] =
//           props.itcPanelProps.targets.lift(p)

//         val selectedResult: Pot[ItcGraphResult] =
//           props.selectedTarget.get.toPot
//             .flatMap: t =>
//               props.itcGraphResults.flatMap(_.asterismGraphs.get(t).flatMap(_.toOption).toPot)

//         val selectedTarget = props.selectedTarget
//         val existTargets   = props.itcPanelProps.targets.nonEmpty && selectedTarget.get.isDefined

//         val itcTargets          = props.itcPanelProps.itcTargets.foldMap(_.toList)
//         val idx                 = itcTargets.indexWhere(props.selectedTarget.get.contains)
//         val itcTargetsWithIndex = itcTargets.zipWithIndex

//         def singleSN: ItcGraphResult => VdomNode =
//           (r: ItcGraphResult) => <.span(formatSN(r.singleSNRatio.value))

//         def totalSN: ItcGraphResult => VdomNode =
//           (r: ItcGraphResult) => <.span(formatSN(r.finalSNRatio.value))

//         def snSection(title: String, fn: ItcGraphResult => VdomNode) =
//           React.Fragment(
//             <.label(title),
//             if (existTargets && props.itcPanelProps.isExecutable)
//               selectedResult.renderPot(
//                 fn,
//                 Icons.Spinner.withSpin(true),
//                 e => <.span(Icons.MissingInfoIcon).withTooltip(e.getMessage)
//               )
//             else
//               <.span(Icons.MissingInfoIcon).withTooltip(MissingInfoMsg)
//           )

//         <.div(
//           ExploreStyles.ItcTileTitle,
//           <.label(s"Target:"),
//           Dropdown(
//             clazz = ExploreStyles.ItcTileTargetSelector,
//             value = idx,
//             onChange = {
//               case t: Int => props.selectedTarget.set(newSelected(t))
//               case _      => Callback.empty
//             },
//             options = itcTargetsWithIndex.map((t, i) => SelectItem(label = t.name.value, value = i))
//           ).when(itcTargets.length > 1),
//           <.span(props.selectedTarget.get.map(_.name.value).getOrElse("-"))
//             .when(itcTargets.length === 1),
//           snSection("S/N per exposure:", singleSN),
//           snSection("S/N Total:", totalSN)
//         )
