// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

// package explore.tabs

// import cats.syntax.all.*
// import eu.timepit.refined.types.string.NonEmptyString
// import explore.components.Tile
// import explore.components.ui.ExploreStyles
// import explore.model.GlobalPreferences
// import explore.targeteditor.plots.ElevationPlotTile
// import explore.targeteditor.plots.PlotData
// import japgolly.scalajs.react.*
// import japgolly.scalajs.react.vdom.html_<^.*
// import lucuma.core.enums.Site
// import lucuma.core.model.TimingWindow
// import lucuma.core.model.User
// import lucuma.ui.syntax.all.given

// import java.time.Duration
// import java.time.Instant

// object ElevationPlotTile:

//   def elevationPlotTile(
//     userId:            Option[User.Id],
//     tileId:            NonEmptyString,
//     plotData:          PlotData,
//     site:              Option[Site],
//     vizTime:           Option[Instant],
//     pendingTime:       Option[Duration],
//     timingWindows:     List[TimingWindow] = List.empty,
//     globalPreferences: GlobalPreferences,
//     emptyMessage:      String
//   ): Tile[Unit] =
//     Tile(
//       tileId,
//       "Elevation Plot",
//       bodyClass = ExploreStyles.ElevationPlotTileBody
//     ) { _ =>
//       userId
//         .map: uid =>
//           ElevationPlotTile(
//             uid,
//             plotData,
//             site,
//             vizTime,
//             pendingTime,
//             timingWindows,
//             globalPreferences,
//             emptyMessage
//           ): VdomNode
//         .getOrElse:
//           <.div(
//             ExploreStyles.FullHeightWidth |+| ExploreStyles.HVCenter |+| ExploreStyles.EmptyTreeContent,
//             <.div("Select a target")
//           )
//     }
