// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.itc

import cats.Eq
import cats.data.EitherNec
import cats.derived.*
import cats.effect.IO
import cats.syntax.all.*
import crystal.*
import crystal.react.*
import crystal.react.hooks.*
import eu.timepit.refined.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.common.UserPreferencesQueries.*
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.Constants
import explore.model.GlobalPreferences
import explore.model.ObsTabTileIds
import explore.model.Observation
import explore.model.itc.*
import explore.model.reusability.given
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.Band
import lucuma.core.math.Wavelength
import lucuma.core.model.SourceProfile
import lucuma.core.model.User
import lucuma.itc.GraphType
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Dropdown
import lucuma.react.primereact.Message
import lucuma.react.primereact.SelectItem
import lucuma.ui.syntax.all.given
import lucuma.ui.syntax.pot.*
import lucuma.ui.utils.*

object ItcTile:
  case class TargetAndResults(
    target: ItcTarget,
    result: Either[ItcQueryProblem, ItcGraphResult]
  ) derives Eq:
    def asTargetProblem: EitherNec[ItcTargetProblem, ItcGraphResult] =
      result.leftMap(p => ItcTargetProblem(target.name.some, p)).toEitherNec

  given Reusability[TargetAndResults] = Reusability.byEq

  extension (tuple: (ItcTarget, Either[ItcQueryProblem, ItcGraphResult]))
    def toTargetAndResults: TargetAndResults =
      TargetAndResults(tuple._1, tuple._2)

  extension (asterismGraphResults: EitherNec[ItcTargetProblem, ItcAsterismGraphResults])
    def targets: List[ItcTarget]                                      =
      asterismGraphResults.toOption.map(_.asterismGraphs.keys.toList).getOrElse(List.empty)
    def findGraphResults(target: ItcTarget): Option[TargetAndResults] =
      asterismGraphResults.toOption
        .flatMap(_.asterismGraphs.get(target))
        .map(TargetAndResults(target, _))
    def brightestTarget: Option[TargetAndResults]                     =
      asterismGraphResults.toOption.flatMap(_.brightestTarget).flatMap(findGraphResults)
    def brightestOrFirst: Option[TargetAndResults]                    =
      brightestTarget
        .orElse(
          asterismGraphResults.toOption
            .flatMap(_.asterismGraphs.headOption)
            .map(_.toTargetAndResults)
        )

  def apply(
    uid:               Option[User.Id],
    oid:               Observation.Id,
    itcGraphResults:   Pot[EitherNec[ItcTargetProblem, ItcAsterismGraphResults]],
    globalPreferences: View[GlobalPreferences]
  ) =
    Tile(
      ObsTabTileIds.ItcId.id,
      s"ITC",
      none[TargetAndResults],
      bodyClass = ExploreStyles.ItcTileBody
    )(
      s =>
        uid.map(
          Body(
            _,
            oid,
            itcGraphResults,
            globalPreferences,
            s
          )
        ),
      (s, _) =>
        Title(
          itcGraphResults.toOption.flatMap(_.toOption),
          s
        )
    )

  private case class Body(
    uid:                      User.Id,
    oid:                      Observation.Id,
    itcGraphResults:          Pot[EitherNec[ItcTargetProblem, ItcAsterismGraphResults]],
    globalPreferences:        View[GlobalPreferences],
    selectedTargetAndResults: View[Option[TargetAndResults]]
  ) extends ReactFnProps(Body)

  private object Body
      extends ReactFnComponent[Body](props =>
        for
          ctx <- useContext(AppContext.ctx)
          _   <- // Reset the selected target if the brightest target changes
            useEffectWhenDepsReadyOrChange(props.itcGraphResults.map(_.brightestOrFirst)):
              itcBrightestOrFirst => props.selectedTargetAndResults.set(itcBrightestOrFirst)
          _   <- // if the targets change, make sure the selected target is still available
            useEffectWhenDepsReadyOrChange(props.itcGraphResults.map(_.targets)): targets =>
              if (props.selectedTargetAndResults.get.exists(targets.contains))
                Callback.empty
              else
                props.selectedTargetAndResults
                  .set(props.itcGraphResults.toOption.flatMap(_.brightestOrFirst))
        yield
          import ctx.given

          def body(signalToNoiseAt: Wavelength, graphResult: ItcGraphResult): VdomNode =
            val globalPreferences: View[GlobalPreferences] =
              props.globalPreferences.withOnMod: prefs =>
                ItcPlotPreferences
                  .updatePlotPreferences[IO](props.uid, prefs.itcChartType, prefs.itcDetailsOpen)
                  .runAsyncAndForget

            val graphTypeView: View[GraphType] =
              globalPreferences.zoom(GlobalPreferences.itcChartType)

            val detailsView: View[PlotDetails] =
              globalPreferences.zoom(GlobalPreferences.itcDetailsOpen)

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

            val sourceProfile: SourceProfile                       = graphResult.target.input.sourceProfile
            val selectedTargetBrightness: Option[BrightnessValues] =
              graphResult.integrationTime.bandOrLine
                .fold(bandValues(sourceProfile), emissionLineValues(sourceProfile))

            <.div(
              ExploreStyles.ItcPlotSection,
              ExploreStyles.ItcPlotDetailsHidden.unless(detailsView.get.value)
            )(
              ItcSpectroscopyPlotDescription(
                selectedTargetBrightness,
                graphResult.itcExposureTime,
                graphResult.ccds,
                graphResult.finalSNRatio,
                graphResult.singleSNRatio
              ),
              ItcSpectroscopyPlot(
                graphResult.ccds,
                graphResult.graphData,
                graphTypeView.get,
                graphResult.target.name.value,
                signalToNoiseAt,
                detailsView.get
              ),
              ItcPlotControl(graphTypeView, detailsView)
            )

          val resultPot: Pot[EitherNec[ItcTargetProblem, (Wavelength, ItcGraphResult)]] =
            props.itcGraphResults.map(
              _.flatMap(agr =>
                props.selectedTargetAndResults.get
                  .toRightNec(ItcQueryProblem.GenericError(Constants.NoTargets).toTargetProblem)
                  .flatMap(_.asTargetProblem)
                  .map(gr => (agr.signalToNoiseAt, gr))
              )
            )

          resultPot.renderPot(
            valueRender = _.fold(
              es =>
                Message(
                  text = es.toList
                    .map(_.format)
                    .mkString("Could not generate a graph:\n", "\n", ""),
                  severity = Message.Severity.Warning
                ),
              (signalToNoiseAt, graphResult) => body(signalToNoiseAt, graphResult)
            )
          )
      )

  private case class Title(
    itcGraphResults:          Option[ItcAsterismGraphResults],
    selectedTargetAndResults: View[Option[TargetAndResults]]
  ) extends ReactFnProps(Title)

  private object Title
      extends ReactFnComponent[Title](props =>
        for {
          options <-
            useMemo(
              props.itcGraphResults.foldMap(_.asterismGraphs.toList.map(_.toTargetAndResults))
            )(
              _.map(t => SelectItem(label = t.target.name.value, value = t))
            )
        } yield
          def singleSN: ItcGraphResult => VdomNode =
            (r: ItcGraphResult) => <.span(formatSN(r.singleSNRatio.value))

          def totalSN: ItcGraphResult => VdomNode =
            (r: ItcGraphResult) => <.span(formatSN(r.finalSNRatio.value))

          def snSection(title: String, fn: ItcGraphResult => VdomNode) =
            props.selectedTargetAndResults.get
              .flatMap(_.result.toOption)
              .map: result =>
                React.Fragment(
                  <.label(title),
                  fn(result)
                )

          // The only way this should be empty is if there are no targets in the results.
          props.selectedTargetAndResults.get.map: gr =>
            <.div(
              ExploreStyles.ItcTileTitle,
              <.label(s"Target:"),
              Dropdown(
                clazz = ExploreStyles.ItcTileTargetSelector,
                value = gr,
                onChange = o => props.selectedTargetAndResults.set(o.some),
                options = options.value
              ).when(options.value.length > 1),
              <.span(props.selectedTargetAndResults.get.map(_.target.name.value).getOrElse("-"))
                .when(options.value.length === 1),
              snSection("S/N per exposure:", singleSN),
              snSection("S/N Total:", totalSN)
            )
      )
