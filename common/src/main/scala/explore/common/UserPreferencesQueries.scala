// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import cats.ApplicativeThrow
import cats.MonadThrow
import cats.Order.*
import cats.data.OptionT
import cats.syntax.all.*
import clue.FetchClient
import clue.data.syntax.*
import eu.timepit.refined.*
import eu.timepit.refined.numeric.*
import explore.DefaultErrorPolicy
import explore.model.AladinFullScreen
import explore.model.AladinMouseScroll
import explore.model.ChartOp
import explore.model.ColorsInverted
import explore.model.TargetVisualOptions
import explore.model.Transformation
import explore.model.UserGlobalPreferences
import explore.model.enums.GridBreakpointName
import explore.model.enums.GridLayoutSection
import explore.model.enums.PlotRange
import explore.model.enums.TableId
import explore.model.enums.TimeDisplay
import explore.model.enums.Visible
import explore.model.itc.PlotDetails
import explore.model.itc.*
import explore.model.layout.*
import explore.model.layout.given
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.model.ObsAttachment
import lucuma.core.model.Observation
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.util.Enumerated
import lucuma.itc.ChartType
import lucuma.react.table.*
import lucuma.refined.*
import lucuma.ui.table.TableStateStore
import org.typelevel.log4cats.Logger
import queries.common.UserPreferencesQueriesGQL.UserTargetPreferencesQuery.Data.ExploreTargetPreferencesByPk
import queries.common.UserPreferencesQueriesGQL.*
import queries.schemas.UserPreferencesDB
import queries.schemas.UserPreferencesDB.Enums.*
import queries.schemas.UserPreferencesDB.Scalars.*
import queries.schemas.UserPreferencesDB.Types.LucumaObservationInsertInput
import queries.schemas.UserPreferencesDB.Types.*
import react.gridlayout.{BreakpointName => _, _}

import scala.collection.immutable.SortedMap

import scalajs.js

object UserPreferencesQueries:
  type TableColumnPreferences = TableColumnPreferencesQuery.Data
  val TableColumnPreferences = TableColumnPreferencesQuery.Data

  object UserPreferences:
    def storePreferences[F[_]: ApplicativeThrow](
      userId:             User.Id,
      aladinMouseScroll:  Option[AladinMouseScroll] = None,
      fullScreen:         Option[AladinFullScreen] = None,
      showCatalog:        Option[Visible] = None,
      agsOverlay:         Option[Visible] = None,
      scienceOffsets:     Option[Visible] = None,
      acquisitionOffsets: Option[Visible] = None
    )(using FetchClient[F, UserPreferencesDB]): F[Unit] =
      UserPreferencesAladinUpdate[F]
        .execute(
          objects = LucumaUserPreferencesInsertInput(
            userId = userId.show.assign,
            aladinMouseScroll = aladinMouseScroll.map(_.value).orIgnore,
            showCatalog = showCatalog.map(_.isVisible).orIgnore,
            agsOverlay = agsOverlay.map(_.isVisible).orIgnore,
            scienceOffsets = scienceOffsets.map(_.isVisible).orIgnore,
            acquisitionOffsets = acquisitionOffsets.map(_.isVisible).orIgnore,
            fullScreen = fullScreen.map(_.value).orIgnore
          )
        )
        .attempt
        .void
  end UserPreferences

  object GridLayouts:
    extension (e: react.gridlayout.BreakpointName)
      def toGridBreakpointName: GridBreakpointName =
        Enumerated[GridBreakpointName].unsafeFromTag(e.name)

    def positions2LayoutMap(
      g: (GridBreakpointName, List[UserGridLayoutQuery.Data.LucumaGridLayoutPositions])
    ): (react.gridlayout.BreakpointName, (Int, Int, Layout)) =
      val bn = breakpointNameFromString(g._1.tag)
      bn -> ((breakpointWidth(bn),
              breakpointCols(bn),
              Layout(
                g._2.map(p => LayoutItem(p.width, p.height, p.x, p.y, p.tile))
              )
      ))

    // Gets the layout of a section.
    // This will return a default in case there is no data or errors
    def queryWithDefault[F[_]: MonadThrow](
      userId:        Option[User.Id],
      layoutSection: GridLayoutSection,
      defaultValue:  LayoutsMap
    )(using FetchClient[F, UserPreferencesDB]): F[LayoutsMap] =
      (for {
        uid <- OptionT.fromOption[F](userId)
        r   <-
          OptionT
            .liftF[F, SortedMap[react.gridlayout.BreakpointName, (Int, Int, Layout)]] {
              UserGridLayoutQuery[F].query(uid.show, layoutSection).map { r =>
                r.lucumaGridLayoutPositions match {
                  case l if l.isEmpty => defaultValue
                  case l              =>
                    SortedMap(l.groupBy(_.breakpointName).map(positions2LayoutMap).toList: _*)
                }
              }
            }
            .handleErrorWith(_ => OptionT.none)
      } yield r).getOrElse(defaultValue)

    def storeLayoutsPreference[F[_]: ApplicativeThrow](
      userId:  Option[User.Id],
      section: GridLayoutSection,
      layouts: Layouts
    )(using FetchClient[F, UserPreferencesDB]): F[Unit] =
      userId.traverse { uid =>
        UserGridLayoutUpsert[F]
          .execute(
            layouts.layouts.flatMap { bl =>
              bl.layout.l.collect {
                case i if i.i.nonEmpty =>
                  LucumaGridLayoutPositionsInsertInput(
                    userId = uid.show.assign,
                    section = section.assign,
                    breakpointName = bl.name.toGridBreakpointName.assign,
                    width = i.w.assign,
                    height = i.h.assign,
                    x = i.x.assign,
                    y = i.y.assign,
                    tile = i.i.getOrElse("").assign
                  )
              }
            }
          )
          .attempt
      }.void
  end GridLayouts

  object TargetPreferences:
    def updateAladinPreferences[F[_]: ApplicativeThrow](
      uid:        User.Id,
      targetId:   Target.Id,
      fovRA:      Option[Angle] = None,
      fovDec:     Option[Angle] = None,
      saturation: Option[Int] = None,
      brightness: Option[Int] = None
    )(using FetchClient[F, UserPreferencesDB]): F[Unit] =
      UserTargetPreferencesUpsert[F]
        .execute(
          LucumaTargetInsertInput(
            targetId = targetId.show.assign,
            lucuma_target_preferences = ExploreTargetPreferencesArrRelInsertInput(
              data = List(
                ExploreTargetPreferencesInsertInput(
                  userId = uid.show.assign,
                  fovRA = fovRA.map(_.toMicroarcseconds).orIgnore,
                  fovDec = fovDec.map(_.toMicroarcseconds).orIgnore,
                  saturation = saturation.orIgnore,
                  brightness = brightness.orIgnore
                )
              ),
              onConflict = ExploreTargetPreferencesOnConflict(
                constraint = ExploreTargetPreferencesConstraint.LucumaTargetPreferencesPkey,
                update_columns = List(
                  ExploreTargetPreferencesUpdateColumn.FovRA.some.filter(_ => fovRA.isDefined),
                  ExploreTargetPreferencesUpdateColumn.FovDec.some.filter(_ => fovDec.isDefined),
                  ExploreTargetPreferencesUpdateColumn.Saturation.some.filter(_ =>
                    saturation.isDefined
                  ),
                  ExploreTargetPreferencesUpdateColumn.Brightness.some.filter(_ =>
                    brightness.isDefined
                  )
                ).flattenOption
              ).assign
            ).assign
          )
        )
        .attempt
        .void

    // Gets the target properties
    def queryWithDefault[F[_]: ApplicativeThrow](
      uid:        User.Id,
      tid:        Target.Id,
      defaultFov: Angle
    )(using
      FetchClient[F, UserPreferencesDB]
    ): F[(UserGlobalPreferences, TargetVisualOptions)] =
      for {
        r <-
          UserTargetPreferencesQuery[F]
            .query(uid.show, tid.show)
            .map { r =>
              val userPrefs   =
                r.lucumaUserPreferencesByPk
              val targetPrefs = r.exploreTargetPreferencesByPk
              (userPrefs, targetPrefs)
            }
            .handleError(_ => (none, none))
      } yield {
        val (userPrefsResult, targetPrefsResult) = r

        val userPrefs = UserGlobalPreferences(
          AladinMouseScroll(userPrefsResult.flatMap(_.aladinMouseScroll).getOrElse(false)),
          AladinFullScreen(userPrefsResult.flatMap(_.fullScreen).getOrElse(false)),
          Visible.value.reverseGet(userPrefsResult.flatMap(_.showCatalog).getOrElse(true)),
          Visible.value.reverseGet(userPrefsResult.flatMap(_.agsOverlay).getOrElse(true)),
          Visible.value.reverseGet(userPrefsResult.flatMap(_.scienceOffsets).getOrElse(true)),
          Visible.value.reverseGet(userPrefsResult.flatMap(_.acquisitionOffsets).getOrElse(true))
        )

        val targetPrefs = {
          val fovRA  =
            targetPrefsResult.flatMap(_.fovRA.map(Angle.fromMicroarcseconds)).getOrElse(defaultFov)
          val fovDec =
            targetPrefsResult.flatMap(_.fovDec.map(Angle.fromMicroarcseconds)).getOrElse(defaultFov)
          val offset = targetPrefsResult
            .flatMap(u =>
              (u.viewOffsetP.map(Angle.fromMicroarcseconds(_).p),
               u.viewOffsetQ.map(Angle.fromMicroarcseconds(_).q)
              )
                .mapN(Offset.apply)
            )
            .getOrElse(Offset.Zero)

          def rangeProp(op: ExploreTargetPreferencesByPk => Option[Int]) = targetPrefsResult
            .flatMap(op)
            .flatMap(refineV[Interval.Closed[0, 100]](_).toOption)
            .getOrElse(100.refined[Interval.Closed[0, 100]])

          val saturation = rangeProp(_.saturation)
          val brightness = rangeProp(_.brightness)

          TargetVisualOptions(fovRA, fovDec, offset, saturation, brightness)
        }
        (userPrefs, targetPrefs)
      }

    def updateViewOffset[F[_]: ApplicativeThrow](
      uid:      User.Id,
      targetId: Target.Id,
      offset:   Offset
    )(using FetchClient[F, UserPreferencesDB]): F[Unit] =
      UserTargetPreferencesUpsert[F]
        .execute(
          LucumaTargetInsertInput(
            targetId = targetId.show.assign,
            lucuma_target_preferences = ExploreTargetPreferencesArrRelInsertInput(
              data = List(
                ExploreTargetPreferencesInsertInput(
                  userId = uid.show.assign,
                  viewOffsetP = offset.p.toAngle.toMicroarcseconds.assign,
                  viewOffsetQ = offset.q.toAngle.toMicroarcseconds.assign
                )
              ),
              onConflict = ExploreTargetPreferencesOnConflict(
                constraint = ExploreTargetPreferencesConstraint.LucumaTargetPreferencesPkey,
                update_columns = List(
                  ExploreTargetPreferencesUpdateColumn.ViewOffsetP.some,
                  ExploreTargetPreferencesUpdateColumn.ViewOffsetQ.some
                ).flattenOption
              ).assign
            ).assign
          )
        )
        .attempt
        .void

  end TargetPreferences

  object FinderChartPreferences:
    // Gets the prefs for the itc plot
    def queryWithDefault[F[_]: ApplicativeThrow](
      oid: Observation.Id,
      aid: ObsAttachment.Id
    )(using FetchClient[F, UserPreferencesDB]): F[Transformation] =
      FinderChartTransformationQuery[F]
        .query(aid.show, oid.show)
        .map { r =>
          r.exploreFinderChartByPk.map(r =>
            Transformation(
              ChartOp.Rotate(r.rotate),
              ChartOp.ScaleX(r.scaleX.toDouble / 100),
              ChartOp.ScaleY(r.scaleY.toDouble / 100),
              ChartOp.FlipX(r.flipX),
              ChartOp.FlipY(r.flipY),
              ColorsInverted.fromBoolean(r.inverted)
            )
          )
        }
        .handleError(_ => none)
        .map(_.getOrElse(Transformation.Default))

    def updateTransformation[F[_]: ApplicativeThrow](
      oid:       Observation.Id,
      aid:       ObsAttachment.Id,
      transform: Transformation
    )(using FetchClient[F, UserPreferencesDB]): F[Unit] =
      FinderChartUpsert[F]
        .execute(
          ExploreFinderChartInsertInput(
            observationId = oid.show.assign,
            attachmentId = aid.show.assign,
            flipX = transform.flipX.flip.assign,
            flipY = transform.flipY.flip.assign,
            rotate = transform.rotate.deg.assign,
            scaleX = (transform.scaleX.scale * 100).toInt.assign,
            scaleY = (transform.scaleY.scale * 100).toInt.assign,
            inverted = transform.inverted.value.assign
          )
        )
        .attempt
        .void
  object ItcPlotPreferences:
    // Gets the prefs for the itc plot
    def queryWithDefault[F[_]: ApplicativeThrow](
      uid: User.Id,
      oid: Observation.Id
    )(using FetchClient[F, UserPreferencesDB]): F[(ChartType, PlotDetails)] =
      for r <-
          ItcPlotPreferencesQuery[F]
            .query(uid.show, oid.show)
            .map { r =>
              r.lucumaItcPlotPreferencesByPk.map(result => (result.chartType, result.detailsOpen))
            }
            .handleError(_ => none)
      yield
        val chartType = r.map(_._1).getOrElse(ChartType.S2NChart)
        val details   = r.map(x => PlotDetails(x._2)).getOrElse(PlotDetails.Shown)

        (chartType, details)

    def updatePlotPreferences[F[_]: ApplicativeThrow](
      uid:       User.Id,
      oid:       Observation.Id,
      chartType: ChartType,
      details:   PlotDetails
    )(using FetchClient[F, UserPreferencesDB]): F[Unit] =
      ItcPlotObservationUpsert[F]
        .execute(
          LucumaObservationInsertInput(
            observationId = oid.show.assign,
            lucuma_itc_plot_preferences = LucumaItcPlotPreferencesArrRelInsertInput(
              data = List(
                LucumaItcPlotPreferencesInsertInput(
                  userId = uid.show.assign,
                  chartType = chartType.assign,
                  detailsOpen = details.value.assign
                )
              ),
              onConflict = LucumaItcPlotPreferencesOnConflict(
                constraint = LucumaItcPlotPreferencesConstraint.LucumaItcPlotPreferencesPkey,
                update_columns = List(
                  LucumaItcPlotPreferencesUpdateColumn.ChartType,
                  LucumaItcPlotPreferencesUpdateColumn.DetailsOpen
                )
              ).assign
            ).assign
          )
        )
        .attempt
        .void

  object ElevationPlotPreference:
    def updatePlotPreferences[F[_]: ApplicativeThrow](
      userId: User.Id,
      range:  PlotRange,
      time:   TimeDisplay
    )(using FetchClient[F, UserPreferencesDB]): F[Unit] =
      UserPreferencesElevPlotUpdate[F]
        .execute(
          userId = userId.show.assign,
          elevationPlotRange = range.assign,
          elevationPlotTime = time.assign
        )
        .attempt
        .void

    // Gets the prefs for the elevation plot
    def queryWithDefault[F[_]: ApplicativeThrow](uid: User.Id)(using
      FetchClient[F, UserPreferencesDB]
    ): F[(PlotRange, TimeDisplay)] =
      for r <-
          UserElevationPlotPreferencesQuery[F]
            .query(uid.show)
            .map { r =>
              r.lucumaUserPreferencesByPk.map(result =>
                (result.elevationPlotRange, result.elevationPlotTime)
              )
            }
            .handleError(_ => none)
      yield
        val range = r.flatMap(_._1).getOrElse(PlotRange.Night)
        val time  = r.flatMap(_._2).getOrElse(TimeDisplay.Site)

        (range, time)

  case class TableStore[F[_]: MonadThrow](
    userId:  Option[User.Id],
    tableId: TableId,
    columns: List[ColumnDef[?, ?]]
  )(using FetchClient[F, UserPreferencesDB], Logger[F])
      extends TableStateStore[F]:
    def load(): F[TableState => TableState] =
      userId
        .traverse { uid =>
          TableColumnPreferencesQuery[F]
            .query(
              userId = uid.show.assign,
              tableId = tableId.assign
            )
            .recoverWith(t =>
              Logger[F]
                .error(t)(s"Error loading table preferences for [$tableId]")
                .as(TableColumnPreferencesQuery.Data(Nil))
            )
            .map(prefs =>
              (tableState: TableState) =>
                tableState
                  .setColumnVisibility(
                    prefs.lucumaTableColumnPreferences.applyVisibility(tableState.columnVisibility)
                  )
                  .setSorting(prefs.lucumaTableColumnPreferences.applySorting(tableState.sorting))
            )
        }
        .map(_.getOrElse(identity))

    def save(state: TableState): F[Unit] =
      userId.traverse { uid =>
        TableColumnPreferencesUpsert[F]
          .execute(
            columns.map(col =>
              val sorting: Map[ColumnId, (SortDirection, Int)] = state.sorting.value.zipWithIndex
                .map((colSort, idx) => colSort.columnId -> (colSort.direction, idx))
                .toMap

              LucumaTableColumnPreferencesInsertInput(
                userId = uid.show.assign,
                tableId = tableId.assign,
                columnId = col.id.value.assign,
                visible =
                  state.columnVisibility.value.getOrElse(col.id, Visibility.Visible).value.assign,
                sorting = sorting.get(col.id).map(_._1).orUnassign,
                sortingOrder = sorting.get(col.id).map(_._2).orUnassign
              )
            )
          )
          .attempt
      }.void
  end TableStore

  extension (tableColsPrefs: List[TableColumnPreferencesQuery.Data.LucumaTableColumnPreferences])
    def applyVisibility(original: ColumnVisibility): ColumnVisibility =
      original.modify(
        _ ++
          tableColsPrefs.map(col => ColumnId(col.columnId) -> Visibility.fromVisible(col.visible))
      )

    def applySorting(original: Sorting): Sorting =
      val sortedCols =
        tableColsPrefs
          .flatMap(col => (ColumnId(col.columnId).some, col.sorting, col.sortingOrder).tupled)
          .sortBy(_._3)

      // We don't force unsorting, in case there's a default sorting.
      sortedCols match
        case Nil      => original
        case nonEmpty => Sorting(nonEmpty.map((colId, dir, _) => colId -> dir): _*)
