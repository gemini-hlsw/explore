// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.itc

import cats.effect.IO
import cats.syntax.all.*
import crystal.*
import crystal.react.*
import crystal.react.hooks.*
import eu.timepit.refined.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.common.UserPreferencesQueries.*
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.Constants
import explore.model.Constants.MissingInfoMsg
import explore.model.GlobalPreferences
import explore.model.ObsTabTileIds
import explore.model.Observation
import explore.model.itc.*
import explore.model.reusability.given
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.Band
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.SourceProfile
import lucuma.core.model.User
import lucuma.core.util.NewType
import lucuma.itc.GraphType
import lucuma.react.common.ReactFnProps
import lucuma.react.floatingui.syntax.*
import lucuma.react.primereact.Dropdown
import lucuma.react.primereact.Message
import lucuma.react.primereact.SelectItem
import lucuma.ui.syntax.all.given
import lucuma.ui.syntax.pot.*
import lucuma.ui.utils.*

object ItcTile:
  def apply(
    uid:               Option[User.Id],
    oid:               Observation.Id,
    itcGraphQuerier:   ItcGraphQuerier,
    itcGraphResults:   Pot[ItcAsterismGraphResults],
    globalPreferences: View[GlobalPreferences]
  ) =
    Tile(
      ObsTabTileIds.ItcId.id,
      s"ITC",
      ItcTileState(itcGraphResults.toOption.flatMap(_.brightestTarget)),
      bodyClass =
        ExploreStyles.ItcTileBody |+| ExploreStyles.ItcTileBodyError.when_(itcGraphResults.isError)
    )(
      s =>
        uid.map(
          Body(
            _,
            oid,
            itcGraphQuerier,
            itcGraphResults,
            globalPreferences,
            s
          )
        ),
      (s, _) =>
        Title(
          itcGraphQuerier,
          itcGraphResults,
          s
        )
    )

  object ItcTileState extends NewType[Option[ItcTarget]]:
    val Initial: ItcTileState = ItcTileState(none)
  type ItcTileState = ItcTileState.Type

  private case class Body(
    uid:               User.Id,
    oid:               Observation.Id,
    itcGraphQuerier:   ItcGraphQuerier,
    itcGraphResults:   Pot[ItcAsterismGraphResults],
    globalPreferences: View[GlobalPreferences],
    tileState:         View[ItcTileState]
  ) extends ReactFnProps(Body.component):
    val selectedTarget: View[Option[ItcTarget]] = tileState.as(ItcTileState.Value)

  private object Body:
    private type Props = Body

    private val component =
      ScalaFnComponent[Props]: props =>
        for
          ctx <- useContext(AppContext.ctx)
          _   <- // Reset the selected target if it changes
            useEffectWhenDepsReadyOrChange(props.itcGraphResults.map(_.brightestTarget)):
              itcBrightestTarget => props.selectedTarget.set(itcBrightestTarget)
        yield
          import ctx.given

          def body(graphResults: ItcAsterismGraphResults): VdomNode =
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

            def bandValues(sp: SourceProfile)(band: Band): Option[BrightnessValues.ForBand] =
              SourceProfile.integratedBrightnesses
                .getOption(sp)
                .flatMap(_.get(band))
                .orElse:
                  SourceProfile.surfaceBrightnesses
                    .getOption(sp)
                    .flatMap(_.get(band))
                .map: b =>
                  BrightnessValues.ForBand(band, b.value, b.units)

            def emissionLineValues(
              sp: SourceProfile
            )(wavelength: Wavelength): Option[BrightnessValues.ForEmissionLine] =
              SourceProfile.integratedWavelengthLines
                .getOption(sp)
                .flatMap(_.get(wavelength))
                .orElse:
                  SourceProfile.surfaceWavelengthLines
                    .getOption(sp)
                    .flatMap(_.get(wavelength))
                .map: e =>
                  BrightnessValues.ForEmissionLine(
                    wavelength,
                    e.lineWidth.value,
                    e.lineFlux.value,
                    e.lineFlux.units
                  )

            val selectedTargetBrightness: Option[BrightnessValues] =
              for
                t         <- selectedTarget
                sp         = t.input.sourceProfile
                r         <- selectedResult
                bandOrLine = r.timeAndGraphs.integrationTime.bandOrLine
                values    <- bandOrLine.fold(bandValues(sp), emissionLineValues(sp))
              yield values

            val isModeSelected: Boolean =
              props.itcGraphQuerier.selectedConfig.isDefined || selectedResult.isDefined

            val targetErrors: Option[String] =
              if graphResults.asterismGraphs.isEmpty then Constants.NoTargets.some
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
                selectedTargetBrightness,
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
                props.itcGraphQuerier.exposureTimeMode.map(ExposureTimeMode.at.get),
                detailsView.get
              ),
              ItcPlotControl(graphTypeView, detailsView)
            )

          props.itcGraphResults.renderPot(
            valueRender = body,
            errorRender = t => Message(text = t.getMessage, severity = Message.Severity.Warning)
          )

  private case class Title(
    itcGraphQuerier: ItcGraphQuerier,
    itcGraphResults: Pot[ItcAsterismGraphResults],
    tileState:       View[ItcTileState]
  ) extends ReactFnProps(Title.component):
    val selectedTarget = tileState.as(ItcTileState.Value)

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
            props.itcGraphQuerier.targets.lift(p)

          val selectedResult: Pot[ItcGraphResult] =
            props.selectedTarget.get.toPot
              .flatMap: t =>
                props.itcGraphResults.flatMap(_.asterismGraphs.get(t).flatMap(_.toOption).toPot)

          val selectedTarget = props.selectedTarget
          val existTargets   = props.itcGraphQuerier.targets.nonEmpty && selectedTarget.get.isDefined

          val itcTargets          = props.itcGraphQuerier.itcTargets.foldMap(_.toList)
          val idx                 = itcTargets.indexWhere(props.selectedTarget.get.contains)
          val itcTargetsWithIndex = itcTargets.zipWithIndex

          def singleSN: ItcGraphResult => VdomNode =
            (r: ItcGraphResult) => <.span(formatSN(r.singleSNRatio.value))

          def totalSN: ItcGraphResult => VdomNode =
            (r: ItcGraphResult) => <.span(formatSN(r.finalSNRatio.value))

          def snSection(title: String, fn: ItcGraphResult => VdomNode) =
            React.Fragment(
              <.label(title),
              if (existTargets && props.itcGraphQuerier.isExecutable)
                selectedResult.renderPot(
                  fn,
                  Icons.Spinner.withSpin(true),
                  e => <.span(Icons.MissingInfoIcon).withTooltip(e.getMessage)
                )
              else
                <.span(Icons.MissingInfoIcon).withTooltip(
                  props.itcGraphQuerier.queryProblemDescription.getOrElse(MissingInfoMsg)
                )
            )

          <.div(
            ExploreStyles.ItcTileTitle,
            <.label(s"Target:"),
            Dropdown(
              clazz = ExploreStyles.ItcTileTargetSelector,
              value = idx,
              onChange = t => props.selectedTarget.set(newSelected(t)),
              options =
                itcTargetsWithIndex.map((t, i) => SelectItem(label = t.name.value, value = i))
            ).when(itcTargets.length > 1),
            <.span(props.selectedTarget.get.map(_.name.value).getOrElse("-"))
              .when(itcTargets.length === 1),
            snSection("S/N per exposure:", singleSN),
            snSection("S/N Total:", totalSN)
          )
