// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import cats.ApplicativeThrow
import cats.MonadThrow
import cats.Order.*
import cats.data.OptionT
import cats.syntax.all.*
import clue.TransactionalClient
import clue.data.syntax.*
import explore.model.AladinMouseScroll
import explore.model.GridLayoutSection
import explore.model.ResizableSection
import explore.model.TargetVisualOptions
import explore.model.UserGlobalPreferences
import explore.model.enums.ItcChartType
import explore.model.enums.PlotRange
import explore.model.enums.TableId
import explore.model.enums.TimeDisplay
import explore.model.enums.Visible
import explore.model.itc.PlotDetails
import explore.model.itc.*
import explore.model.layout.*
import explore.model.layout.given
import lucuma.core.enums.Site
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.model.Observation
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.react.table.*
import lucuma.ui.table.TableStateStore
import org.scalablytyped.runtime.StringDictionary
import org.typelevel.log4cats.Logger
import queries.common.UserPreferencesQueriesGQL.*
import queries.schemas.UserPreferencesDB
import queries.schemas.UserPreferencesDB.Enums.*
import queries.schemas.UserPreferencesDB.Scalars.*
import queries.schemas.UserPreferencesDB.Types.LucumaObservationInsertInput
import queries.schemas.UserPreferencesDB.Types.*
import queries.schemas.odb.ODBConversions.*
import react.gridlayout.{BreakpointName => _, _}
import reactST.highcharts.highchartsStrings.chart_
import reactST.{tanstackTableCore => raw}

import scala.collection.immutable.SortedMap
import scala.scalajs.js.WrappedDictionary

import scalajs.js.JSConverters.*
import scalajs.js

case class WidthUpsertInput(user: User.Id, section: ResizableSection, width: Int)

object UserPreferencesQueries:
  type TableColumnPreferences = TableColumnPreferencesQuery.Data
  val TableColumnPreferences = TableColumnPreferencesQuery.Data

  object AreaWidths:

    def storeWidthPreference[F[_]: ApplicativeThrow](
      userId:  Option[User.Id],
      section: ResizableSection,
      width:   Int
    )(using TransactionalClient[F, UserPreferencesDB]): F[Unit] =
      import UserWidthsCreation.*

      userId.traverse { i =>
        execute[F](WidthUpsertInput(i, section, width).toInput).attempt
      }.void

    // Gets the width of a section.
    // This is coded to return a default in case
    // there is no data or errors
    def queryWithDefault[F[_]: MonadThrow](
      userId:       Option[User.Id],
      area:         ResizableSection,
      defaultValue: Int
    )(using TransactionalClient[F, UserPreferencesDB]): F[Int] =
      import UserAreaWidths.*
      (for {
        uid <- OptionT.fromOption[F](userId)
        w   <-
          OptionT
            .liftF[F, Option[Int]] {
              query[F](uid.show, area.value)
                .map { r =>
                  r.lucumaResizableWidthByPk.map(_.width)
                }
                .recover(_ => none)
            }
      } yield w).value.map(_.flatten.getOrElse(defaultValue))
  end AreaWidths

  object UserPreferences:
    def storePreferences[F[_]: ApplicativeThrow](
      userId:            User.Id,
      aladinMouseScroll: AladinMouseScroll
    )(using TransactionalClient[F, UserPreferencesDB]): F[Unit] =
      import UserPreferencesAladinUpdate.*

      execute[F](
        userId = userId.show.assign,
        aladinMouseScroll = aladinMouseScroll.value.assign
      ).attempt.void
  end UserPreferences

  object GridLayouts:

    def positions2LayoutMap(
      g: (BreakpointName, List[UserGridLayoutQuery.Data.LucumaGridLayoutPositions])
    ): (react.gridlayout.BreakpointName, (Int, Int, Layout)) =
      import UserGridLayoutUpsert.*
      val bn = breakpointNameFromString(g._1)
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
      resizableArea: ResizableSection,
      defaultValue:  (Int, LayoutsMap)
    )(using TransactionalClient[F, UserPreferencesDB]): F[(Int, LayoutsMap)] =
      (for {
        uid <- OptionT.fromOption[F](userId)
        c   <-
          OptionT.pure(
            LucumaGridLayoutPositionsBoolExp(
              userId = StringComparisonExp(uid.show.assign).assign,
              section = GridLayoutAreaComparisonExp(layoutSection.value.assign).assign
            )
          )
        r   <-
          OptionT
            .liftF[F, (Int, SortedMap[react.gridlayout.BreakpointName, (Int, Int, Layout)])] {
              UserGridLayoutQuery.query[F](uid.show, c, resizableArea.value).map { r =>
                (r.lucumaResizableWidthByPk.map(_.width), r.lucumaGridLayoutPositions) match {
                  case (w, l) if l.isEmpty => (w.getOrElse(defaultValue._1), defaultValue._2)
                  case (w, l)              =>
                    (w.getOrElse(defaultValue._1),
                     SortedMap(l.groupBy(_.breakpointName).map(positions2LayoutMap).toList: _*)
                    )
                }
              }
            }
            .handleErrorWith(_ => OptionT.none)
      } yield r).getOrElse(defaultValue)

    def storeLayoutsPreference[F[_]: ApplicativeThrow](
      userId:  Option[User.Id],
      section: GridLayoutSection,
      layouts: Layouts
    )(using TransactionalClient[F, UserPreferencesDB]): F[Unit] =
      import UserGridLayoutUpsert.*
      userId.traverse { uid =>
        execute[F](
          layouts.layouts.flatMap { bl =>
            bl.layout.l.collect {
              case i if i.i.nonEmpty =>
                LucumaGridLayoutPositionsInsertInput(
                  userId = uid.show.assign,
                  section = section.value.assign,
                  breakpointName = bl.name.name.assign,
                  width = i.w.assign,
                  height = i.h.assign,
                  x = i.x.assign,
                  y = i.y.assign,
                  tile = i.i.getOrElse("").assign
                )
            }
          }
        ).attempt
      }.void
  end GridLayouts

  object TargetPreferences:
    def updateAladinPreferences[F[_]: ApplicativeThrow](
      uid:           User.Id,
      targetId:      Target.Id,
      fovRA:         Option[Angle] = None,
      fovDec:        Option[Angle] = None,
      agsCandidates: Option[Visible] = None,
      agsOverlay:    Option[Visible] = None,
      fullScreen:    Option[Boolean] = None
    )(using TransactionalClient[F, UserPreferencesDB]): F[Unit] =
      import UserTargetPreferencesUpsert.*

      execute[F](
        LucumaTargetInsertInput(
          targetId = targetId.show.assign,
          lucuma_target_preferences = LucumaTargetPreferencesArrRelInsertInput(
            data = List(
              LucumaTargetPreferencesInsertInput(
                userId = uid.show.assign,
                fovRA = fovRA.map(_.toMicroarcseconds).orIgnore,
                fovDec = fovDec.map(_.toMicroarcseconds).orIgnore,
                agsCandidates = agsCandidates.map(Visible.boolIso.reverseGet).orIgnore,
                agsOverlay = agsOverlay.map(Visible.boolIso.reverseGet).orIgnore,
                fullScreen = fullScreen.orIgnore
              )
            ),
            onConflict = LucumaTargetPreferencesOnConflict(
              constraint = LucumaTargetPreferencesConstraint.LucumaTargetPreferencesPkey,
              update_columns = List(
                LucumaTargetPreferencesUpdateColumn.FovRA.some.filter(_ => fovRA.isDefined),
                LucumaTargetPreferencesUpdateColumn.FovDec.some.filter(_ => fovDec.isDefined),
                LucumaTargetPreferencesUpdateColumn.AgsCandidates.some.filter(_ =>
                  agsCandidates.isDefined
                ),
                LucumaTargetPreferencesUpdateColumn.AgsOverlay.some.filter(_ =>
                  agsCandidates.isDefined
                ),
                LucumaTargetPreferencesUpdateColumn.FullScreen.some.filter(_ =>
                  agsCandidates.isDefined
                )
              ).flattenOption
            ).assign
          ).assign
        )
      ).attempt.void

    // Gets the target properties
    def queryWithDefault[F[_]: ApplicativeThrow](
      uid:        User.Id,
      tid:        Target.Id,
      defaultFov: Angle
    )(using
      TransactionalClient[F, UserPreferencesDB]
    ): F[(UserGlobalPreferences, TargetVisualOptions)] =
      import UserTargetPreferencesQuery.*

      for {
        r <-
          query[F](uid.show, tid.show)
            .map { r =>
              val userPrefs   =
                r.lucumaUserPreferencesByPk.flatMap(result => result.aladinMouseScroll)
              val targetPrefs = r.lucumaTargetPreferencesByPk.map(result =>
                (result.fovRA,
                 result.fovDec,
                 result.viewOffsetP,
                 result.viewOffsetQ,
                 result.agsCandidates,
                 result.agsOverlay,
                 result.fullScreen
                )
              )
              (userPrefs, targetPrefs)
            }
            .handleError(_ => (none, none))
      } yield {
        val userPrefs   = UserGlobalPreferences(AladinMouseScroll(r._1.getOrElse(false)))
        val targetPrefs = {
          val fovRA  = r._2.flatMap(_._1.map(Angle.fromMicroarcseconds)).getOrElse(defaultFov)
          val fovDec = r._2.flatMap(_._2.map(Angle.fromMicroarcseconds)).getOrElse(defaultFov)
          val offset = r._2
            .flatMap(u =>
              (u._3.map(Angle.fromMicroarcseconds(_).p), u._4.map(Angle.fromMicroarcseconds(_).q))
                .mapN(Offset.apply)
            )
            .getOrElse(Offset.Zero)

          val agsCandidates = r._2.flatMap(_._5).map(Visible.boolIso.get).getOrElse(Visible.Inline)
          val agsOverlay    = r._2.flatMap(_._6).map(Visible.boolIso.get).getOrElse(Visible.Inline)
          val fullScreen    = r._2.flatMap(_._7).getOrElse(false)

          TargetVisualOptions(fovRA, fovDec, offset, agsCandidates, agsOverlay, fullScreen)
        }
        (userPrefs, targetPrefs)
      }

    def updateViewOffset[F[_]: ApplicativeThrow](
      uid:      User.Id,
      targetId: Target.Id,
      offset:   Offset
    )(using TransactionalClient[F, UserPreferencesDB]): F[Unit] =
      import UserTargetPreferencesUpsert.*

      execute[F](
        LucumaTargetInsertInput(
          targetId = targetId.show.assign,
          lucuma_target_preferences = LucumaTargetPreferencesArrRelInsertInput(
            data = List(
              LucumaTargetPreferencesInsertInput(
                userId = uid.show.assign,
                viewOffsetP = offset.p.toAngle.toMicroarcseconds.assign,
                viewOffsetQ = offset.q.toAngle.toMicroarcseconds.assign
              )
            ),
            onConflict = LucumaTargetPreferencesOnConflict(
              constraint = LucumaTargetPreferencesConstraint.LucumaTargetPreferencesPkey,
              update_columns = List(
                LucumaTargetPreferencesUpdateColumn.ViewOffsetP.some,
                LucumaTargetPreferencesUpdateColumn.ViewOffsetQ.some
              ).flattenOption
            ).assign
          ).assign
        )
      ).attempt.void

  end TargetPreferences

  object ItcPlotPreferences:
    // Gets the prefs for the itc plot
    def queryWithDefault[F[_]: ApplicativeThrow](
      uid: User.Id,
      oid: Observation.Id
    )(using TransactionalClient[F, UserPreferencesDB]): F[(ItcChartType, PlotDetails)] =
      import ItcPlotPreferencesQuery.*

      for r <-
          query[F](uid.show, oid.show)
            .map { r =>
              r.lucumaItcPlotPreferencesByPk.map(result => (result.chartType, result.detailsOpen))
            }
            .handleError(_ => none)
      yield
        val chartType = r.map(_._1).getOrElse(ItcChartType.S2NChart)
        val details   = r.map(x => PlotDetails(x._2)).getOrElse(PlotDetails.Shown)

        (chartType, details)

    def updatePlotPreferences[F[_]: ApplicativeThrow](
      uid:       User.Id,
      oid:       Observation.Id,
      chartType: ItcChartType,
      details:   PlotDetails
    )(using TransactionalClient[F, UserPreferencesDB]): F[Unit] =
      import ItcPlotObservationUpsert.*
      execute[F](
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
      ).attempt.void

  object ElevationPlotPreference:
    def updatePlotPreferences[F[_]: ApplicativeThrow](
      userId: User.Id,
      range:  PlotRange,
      time:   TimeDisplay
    )(using TransactionalClient[F, UserPreferencesDB]): F[Unit] =
      import UserPreferencesElevPlotUpdate.*

      execute[F](
        userId = userId.show.assign,
        elevationPlotRange = range.assign,
        elevationPlotTime = time.assign
      ).attempt.void

    // Gets the prefs for the elevation plot
    def queryWithDefault[F[_]: ApplicativeThrow](uid: User.Id)(using
      TransactionalClient[F, UserPreferencesDB]
    ): F[(PlotRange, TimeDisplay)] =
      import UserElevationPlotPreferencesQuery.*
      for r <-
          query[F](uid.show)
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
  )(using TransactionalClient[F, UserPreferencesDB], Logger[F])
      extends TableStateStore[F]:
    def load(): F[TableState => TableState] =
      userId
        .traverse { uid =>
          TableColumnPreferencesQuery
            .query[F](
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
        TableColumnPreferencesUpsert
          .execute[F](
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

  extension (w: WidthUpsertInput)
    def toInput: LucumaResizableWidthInsertInput =
      LucumaResizableWidthInsertInput(
        w.section.value.assign,
        w.user.toString.assign,
        w.width.assign
      )

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
