// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.itc

import cats.Eq
import cats.data.EitherNec
import cats.derived.*
import cats.syntax.all.*
import crystal.react.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.model.ObsTabTileIds
import explore.model.Observation
import explore.model.itc.*
import explore.model.reusability.given
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.User
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.ui.syntax.all.given
import lucuma.ui.table.*
import explore.modes.ItcInstrumentConfig

object ItcImagingTile:
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
    uid:     Option[User.Id],
    oid:     Observation.Id,
    configs: Option[List[ItcInstrumentConfig]]
  ) =
    Tile(
      ObsTabTileIds.ItcId.id,
      s"ITC",
      ItcTileState.Empty,
      bodyClass = ExploreStyles.ItcImagingTileBody
    )(
      s =>
        uid.map(
          Body(
            _,
            oid,
            configs,
            s
          )
        ),
      (s, _) =>
        Title(
          configs,
          s
        )
    )

  private case class ConfigRow(config: ItcInstrumentConfig)

  private val ColDef = ColumnDef[ConfigRow]

  private val FilterColId     = ColumnId("filter")
  private val InstrumentColId = ColumnId("instrument")
  private val SNColId         = ColumnId("sn")
  private val ExpTimeColId    = ColumnId("exptime")
  private val ExposuresColId  = ColumnId("exposures")

  private val filterColDef =
    ColDef(
      FilterColId,
      _.config.filterStr,
      "Filter"
    ).withSize(120.toPx)

  private val instrumentColDef =
    ColDef(
      InstrumentColId,
      _.config.instrument.shortName,
      "Instrument"
    ).withSize(120.toPx)

  private val snColDef =
    ColDef(
      SNColId,
      _ => "-",
      "S/N"
    ).withSize(80.toPx)

  private val expTimeColDef =
    ColDef(
      ExpTimeColId,
      _ => "-",
      "Exp. Time"
    ).withSize(100.toPx)

  private val exposuresColDef =
    ColDef(
      ExposuresColId,
      _ => "-",
      "Exposures"
    ).withSize(100.toPx)

  private val columns
    : Reusable[List[ColumnDef[ConfigRow, ?, Nothing, Nothing, Nothing, Nothing, Nothing]]] =
    Reusable.always(
      List(filterColDef, instrumentColDef, snColDef, expTimeColDef, exposuresColDef)
    )

  private case class Body(
    uid:       User.Id,
    oid:       Observation.Id,
    configs:   Option[List[ItcInstrumentConfig]],
    tileState: View[ItcTileState]
  ) extends ReactFnProps(Body)

  private object Body
      extends ReactFnComponent[Body](props =>
        for {
          rows  <- useMemo(props.configs.getOrElse(List.empty))(_.map(ConfigRow.apply))
          table <- useReactTable(
                     TableOptions(
                       columns,
                       rows,
                       getRowId = (row, _, _) => RowId(row.config.hashCode.toString),
                       enableSorting = false,
                       enableColumnResizing = false
                     )
                   )
        } yield <.div(
          ExploreStyles.ItcTileBody,
          if (props.configs.exists(_.nonEmpty))
            PrimeTable(table, compact = Compact.Very)
          else
            <.p("No configurations available for ITC calculations")
        )
      )

  private case class Title(
    configs:   Option[List[ItcInstrumentConfig]],
    tileState: View[ItcTileState]
  ) extends ReactFnProps(Title)

  private object Title
      extends ReactFnComponent[Title](props =>
        // for {
        //   options <-
        //     useMemo(
        //       props.itcGraphResults.foldMap(_.asterismGraphs.toList.map(_.toTargetAndResults))
        //     )(
        //       _.map(t => SelectItem(label = t.target.name.value, value = t))
        //     )
        // } yield
        <.div(s"img title ${props.configs}")
        // The only way this should be empty is if there are no targets in the results.
        //   props.selectedTargetAndResults.get.map: gr =>
        //     <.div(
        //       ExploreStyles.ItcTileTitle,
        //       <.label(s"Target:"),
        //       Dropdown(
        //         clazz = ExploreStyles.ItcTileTargetSelector,
        //         value = gr,
        //         onChange = o => props.selectedTargetAndResults.set(o.some),
        //         options = options.value
        //       ).when(options.value.length > 1),
        //       <.span(props.selectedTargetAndResults.get.map(_.target.name.value).getOrElse("-"))
        //         .when(options.value.length === 1)
        //     )
      )
