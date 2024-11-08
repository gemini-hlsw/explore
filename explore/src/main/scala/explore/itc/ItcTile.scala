// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.itc

import cats.effect.IO
import cats.syntax.all.*
import crystal.*
import crystal.react.*
import crystal.react.hooks.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.common.UserPreferencesQueries
import explore.common.UserPreferencesQueries.*
import explore.model.AppContext
import explore.model.itc.*
import lucuma.core.util.NewType
import lucuma.itc.GraphType
import explore.components.Tile
import explore.model.GlobalPreferences
import explore.model.ObsTabTileIds
import explore.model.Observation
import explore.model.TargetList
import lucuma.core.model.User
import eu.timepit.refined.*
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.model.Constants.MissingInfoMsg
import explore.model.itc.ItcAsterismGraphResults
import explore.model.itc.ItcGraphResult
import explore.model.itc.ItcTarget
import explore.model.reusability.given
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.common.ReactFnProps
import lucuma.react.floatingui.syntax.*
import lucuma.react.primereact.Dropdown
import lucuma.react.primereact.SelectItem
import lucuma.ui.syntax.all.given
import lucuma.ui.syntax.pot.*
import lucuma.ui.utils.*

object ItcTile:
  def apply(
    uid:               Option[User.Id],
    oid:               Observation.Id,
    allTargets:        TargetList,
    itcProps:          ItcProps,
    itcGraphResults:   Pot[ItcAsterismGraphResults],
    globalPreferences: View[GlobalPreferences]
  ) =
    Tile(
      ObsTabTileIds.ItcId.id,
      s"ITC",
      TileState(itcGraphResults.toOption.flatMap(_.brightestTarget)),
      bodyClass =
        ExploreStyles.ItcTileBody |+| ExploreStyles.ItcTileBodyError.when_(itcGraphResults.isError)
    )(
      s =>
        uid.map(
          Body(
            _,
            oid,
            itcProps,
            itcGraphResults,
            globalPreferences,
            s
          )
        ),
      (s, _) =>
        Title(
          itcProps,
          itcGraphResults,
          s
        )
    )

  object TileState extends NewType[Option[ItcTarget]]:
    val Initial: TileState = TileState(none)
  type TileState = TileState.Type

  private case class Body(
    uid:               User.Id,
    oid:               Observation.Id,
    itcProps:          ItcProps,
    itcGraphResults:   Pot[ItcAsterismGraphResults],
    globalPreferences: View[GlobalPreferences],
    tileState:         View[TileState]
  ) extends ReactFnProps(Body.component):
    val selectedTarget: View[Option[ItcTarget]] = tileState.zoom(TileState.value.asLens)

  private object Body:
    private type Props = Body

    private val component =
      ScalaFnComponent
        .withHooks[Props]
        .useContext(AppContext.ctx)
        // Reset the selected target if it changes
        .useEffectWhenDepsReadyOrChangeBy((props, _) =>
          props.itcGraphResults.map(_.brightestTarget)
        ): (props, _) =>
          itcBrightestTarget => props.selectedTarget.set(itcBrightestTarget)
        .render: (props, ctx) =>
          import ctx.given

          props.itcGraphResults.renderPot: graphResults =>
            val globalPreferences: View[GlobalPreferences] =
              props.globalPreferences.withOnMod: prefs =>
                ItcPlotPreferences
                  .updatePlotPreferences[IO](props.uid, prefs.itcChartType, prefs.itcDetailsOpen)
                  .runAsyncAndForget

            val graphTypeView: View[GraphType] =
              globalPreferences.zoom(GlobalPreferences.itcChartType)

            val detailsView: View[PlotDetails] =
              globalPreferences.zoom(GlobalPreferences.itcDetailsOpen)

            val selectedTarget: Option[ItcTarget] = props.selectedTarget.get

            val selectedResult: Option[ItcGraphResult] =
              for
                t <- selectedTarget
                r <- graphResults.asterismGraphs.get(t)
                c <- r.toOption
              yield c

            val isModeSelected: Boolean =
              props.itcProps.selectedConfig.isDefined || selectedResult.isDefined

            val targetErrors: Option[String] =
              if graphResults.asterismGraphs.isEmpty then "No target available".some
              else
                NonEmptyString
                  .from:
                    graphResults.asterismGraphs
                      .collect:
                        case (t, Left(e)) => s"${t.name.value}: ${e.message}"
                      .mkString("/n")
                  .toOption
                  .map(_.value)
                  .orElse:
                    "Select a mode to plot".some.filterNot(_ => isModeSelected)

            val error: Option[String] =
              selectedTarget
                .fold(targetErrors): t =>
                  graphResults.asterismGraphs
                    .get(t)
                    .flatMap:
                      _.left.toOption.map(_.message)
                .orElse(targetErrors)

            <.div(
              ExploreStyles.ItcPlotSection,
              ExploreStyles.ItcPlotDetailsHidden.unless(detailsView.get.value)
            )(
              ItcSpectroscopyPlotDescription(
                selectedTarget.flatMap(props.itcProps.targetBrightness),
                selectedResult.map(_.itcExposureTime),
                selectedResult.map(_.ccds),
                selectedResult.map(_.finalSNRatio),
                selectedResult.map(_.singleSNRatio)
              ),
              ItcSpectroscopyPlot(
                selectedResult.map(_.ccds),
                selectedResult.map(_.graphData),
                error,
                graphTypeView.get,
                selectedTarget.map(_.name.value),
                props.itcProps.signalToNoiseAt,
                detailsView.get
              ),
              ItcPlotControl(graphTypeView, detailsView)
            )

  private case class Title(
    itcPanelProps:   ItcProps,
    itcGraphResults: Pot[ItcAsterismGraphResults],
    tileState:       View[TileState]
  ) extends ReactFnProps(Title.component):
    val selectedTarget = tileState.zoom(TileState.value.asLens)

  private object Title:
    private type Props = Title

    private val component =
      ScalaFnComponent
        .withHooks[Props]
        // Reset the selected target if it changes
        .useEffectWhenDepsReadyOrChangeBy(props => props.itcGraphResults.map(_.brightestTarget)):
          props => itcBrightestTarget => props.selectedTarget.set(itcBrightestTarget)
        .render: props =>
          def newSelected(p: Int): Option[ItcTarget] =
            props.itcPanelProps.targets.lift(p)

          val selectedResult: Pot[ItcGraphResult] =
            props.selectedTarget.get.toPot
              .flatMap: t =>
                props.itcGraphResults.flatMap(_.asterismGraphs.get(t).flatMap(_.toOption).toPot)

          val selectedTarget = props.selectedTarget
          val existTargets   = props.itcPanelProps.targets.nonEmpty && selectedTarget.get.isDefined

          val itcTargets          = props.itcPanelProps.itcTargets.foldMap(_.toList)
          val idx                 = itcTargets.indexWhere(props.selectedTarget.get.contains)
          val itcTargetsWithIndex = itcTargets.zipWithIndex

          def singleSN: ItcGraphResult => VdomNode =
            (r: ItcGraphResult) => <.span(formatSN(r.singleSNRatio.value))

          def totalSN: ItcGraphResult => VdomNode =
            (r: ItcGraphResult) => <.span(formatSN(r.finalSNRatio.value))

          def snSection(title: String, fn: ItcGraphResult => VdomNode) =
            React.Fragment(
              <.label(title),
              if (existTargets && props.itcPanelProps.isExecutable)
                selectedResult.renderPot(
                  fn,
                  Icons.Spinner.withSpin(true),
                  e => <.span(Icons.MissingInfoIcon).withTooltip(e.getMessage)
                )
              else
                <.span(Icons.MissingInfoIcon).withTooltip(MissingInfoMsg)
            )

          <.div(
            ExploreStyles.ItcTileTitle,
            <.label(s"Target:"),
            Dropdown(
              clazz = ExploreStyles.ItcTileTargetSelector,
              value = idx,
              onChange = {
                case t: Int => props.selectedTarget.set(newSelected(t))
                case _      => Callback.empty
              },
              options =
                itcTargetsWithIndex.map((t, i) => SelectItem(label = t.name.value, value = i))
            ).when(itcTargets.length > 1),
            <.span(props.selectedTarget.get.map(_.name.value).getOrElse("-"))
              .when(itcTargets.length === 1),
            snSection("S/N per exposure:", singleSN),
            snSection("S/N Total:", totalSN)
          )
