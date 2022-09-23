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
import explore.model.GridLayoutSection
import explore.model.ResizableSection
import explore.model.enums.ItcChartType
import explore.model.enums.PlotRange
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
import queries.common.UserPreferencesQueriesGQL.*
import queries.schemas.UserPreferencesDB
import queries.schemas.UserPreferencesDB.Enums.*
import queries.schemas.UserPreferencesDB.Scalars.*
import queries.schemas.UserPreferencesDB.Types.LucumaObservationInsertInput
import queries.schemas.UserPreferencesDB.Types.*
import queries.schemas.WidthUpsertInput
import queries.schemas.implicits.*
import react.gridlayout.{BreakpointName => _, _}
import reactST.highcharts.highchartsStrings.chart_

import scala.collection.immutable.SortedMap

object UserPreferencesQueries {

  implicit class UserWidthsCreationOps(val self: UserWidthsCreation.type) extends AnyVal {
    import self.*

    def storeWidthPreference[F[_]: ApplicativeThrow](
      userId:  Option[User.Id],
      section: ResizableSection,
      width:   Int
    )(implicit
      cl:      TransactionalClient[F, UserPreferencesDB]
    ): F[Unit] =
      userId.traverse { i =>
        execute[F](WidthUpsertInput(i, section, width)).attempt
      }.void
  }

  extension (self: UserAreaWidths.type)
    // Gets the width of a section.
    // This is coded to return a default in case
    // there is no data or errors
    def queryWithDefault[F[_]: MonadThrow](
      userId:       Option[User.Id],
      area:         ResizableSection,
      defaultValue: Int
    )(using TransactionalClient[F, UserPreferencesDB]): F[Int] =
      import self.*
      (for {
        uid <- OptionT.fromOption[F](userId)
        w   <-
          OptionT
            .liftF[F, Option[Int]] {
              query[F](uid.show, area.value)
                .map { r =>
                  r.explore_resizable_width_by_pk.map(_.width)
                }
                .recover(_ => none)
            }
      } yield w).value.map(_.flatten.getOrElse(defaultValue))

  extension (self: UserGridLayoutQuery.type)
    def positions2LayoutMap(
      g: (BreakpointName, List[UserGridLayoutQuery.Data.GridLayoutPositions])
    ): (react.gridlayout.BreakpointName, (Int, Int, Layout)) =
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
            GridLayoutPositionsBoolExp(
              user_id = StringComparisonExp(uid.show.assign).assign,
              section = GridLayoutAreaComparisonExp(layoutSection.value.assign).assign
            )
          )
        r   <-
          OptionT
            .liftF[F, (Int, SortedMap[react.gridlayout.BreakpointName, (Int, Int, Layout)])] {
              UserGridLayoutQuery.query[F](uid.show, c, resizableArea.value).map { r =>
                (r.explore_resizable_width_by_pk.map(_.width), r.grid_layout_positions) match {
                  case (w, l) if l.isEmpty => (w.getOrElse(defaultValue._1), defaultValue._2)
                  case (w, l)              =>
                    (w.getOrElse(defaultValue._1),
                     SortedMap(l.groupBy(_.breakpoint_name).map(positions2LayoutMap).toList: _*)
                    )
                }
              }
            }
            .handleErrorWith(_ => OptionT.none)
      } yield r).getOrElse(defaultValue)

  implicit class UserGridLayoutUpsertOps(val self: UserGridLayoutUpsert.type) extends AnyVal {
    import self.*

    def storeLayoutsPreference[F[_]: ApplicativeThrow](
      userId:  Option[User.Id],
      section: GridLayoutSection,
      layouts: Layouts
    )(using TransactionalClient[F, UserPreferencesDB]): F[Unit] =
      userId.traverse { uid =>
        execute[F](
          layouts.layouts.flatMap { bl =>
            bl.layout.l.collect {
              case i if i.i.nonEmpty =>
                GridLayoutPositionsInsertInput(
                  user_id = uid.show.assign,
                  section = section.value.assign,
                  breakpoint_name = bl.name.name.assign,
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
  }

  implicit class UserTargetPreferencesQueryOps(val self: UserTargetPreferencesQuery.type)
      extends AnyVal {
    import self.*

    // Gets the layout of a section.
    // This is coded to return a default in case
    // there is no data or errors
    def queryWithDefault[F[_]: ApplicativeThrow](
      uid:        User.Id,
      tid:        Target.Id,
      defaultFov: Angle
    )(implicit
      cl:         TransactionalClient[F, UserPreferencesDB]
    ): F[(Angle, Offset, Visible, Visible, Boolean)] =
      for {
        r <-
          query[F](uid.show, tid.show)
            .map { r =>
              r.lucuma_target_preferences_by_pk.map(result =>
                (result.fov,
                 result.viewOffsetP,
                 result.viewOffsetQ,
                 result.agsCandidates,
                 result.agsOverlay,
                 result.fullScreen
                )
              )
            }
            .handleError(_ => none)
      } yield {
        val fov    = r.map(u => Angle.fromMicroarcseconds(u._1)).getOrElse(defaultFov)
        val offset = r
          .map(u => Offset(Angle.fromMicroarcseconds(u._2).p, Angle.fromMicroarcseconds(u._3).q))
          .getOrElse(Offset.Zero)

        val agsCandidates = r.map(_._4).map(Visible.boolIso.get).getOrElse(Visible.Hidden)
        val agsOverlay    = r.map(_._5).map(Visible.boolIso.get).getOrElse(Visible.Hidden)
        val fullScreen    = r.map(_._6).getOrElse(false)

        (fov, offset, agsCandidates, agsOverlay, fullScreen)
      }
  }

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
              r.lucuma_itc_plot_preferences_by_pk.map(result =>
                (result.chart_type, result.details_open)
              )
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
          observation_id = oid.show.assign,
          lucuma_itc_plot_preferences = LucumaItcPlotPreferencesArrRelInsertInput(
            data = List(
              LucumaItcPlotPreferencesInsertInput(
                user_id = uid.show.assign,
                chart_type = chartType.assign,
                details_open = details.value.assign
              )
            ),
            on_conflict = LucumaItcPlotPreferencesOnConflict(
              constraint = LucumaItcPlotPreferencesConstraint.LucumaItcPlotPreferencesPkey,
              update_columns = List(LucumaItcPlotPreferencesUpdateColumn.ChartType,
                                    LucumaItcPlotPreferencesUpdateColumn.DetailsOpen
              )
            ).assign
          ).assign
        )
      ).attempt.void

  extension (self: UserTargetPreferencesUpsert.type)
    def updateAladinPreferences[F[_]: ApplicativeThrow](
      uid:           User.Id,
      targetId:      Target.Id,
      fov:           Angle,
      agsCandidates: Visible,
      agsOverlay:    Visible,
      fullScreen:    Boolean
    )(using TransactionalClient[F, UserPreferencesDB]): F[Unit] =
      import self.*
      execute[F](
        LucumaTargetInsertInput(
          target_id = targetId.show.assign,
          lucuma_target_preferences = LucumaTargetPreferencesArrRelInsertInput(
            data = List(
              LucumaTargetPreferencesInsertInput(
                user_id = uid.show.assign,
                fov = fov.toMicroarcseconds.assign,
                agsCandidates = Visible.boolIso.reverseGet(agsCandidates).assign,
                agsOverlay = Visible.boolIso.reverseGet(agsOverlay).assign,
                fullScreen = fullScreen.assign
              )
            ),
            on_conflict = LucumaTargetPreferencesOnConflict(
              constraint = LucumaTargetPreferencesConstraint.LucumaTargetPreferencesPkey,
              update_columns = List(
                LucumaTargetPreferencesUpdateColumn.Fov,
                LucumaTargetPreferencesUpdateColumn.AgsCandidates,
                LucumaTargetPreferencesUpdateColumn.AgsOverlay,
                LucumaTargetPreferencesUpdateColumn.FullScreen
              )
            ).assign
          ).assign
        )
      ).attempt.void

  object ElevationPlotPreference:
    def updatePlotPreferences[F[_]: ApplicativeThrow](
      uid:      User.Id,
      targetId: Target.Id,
      site:     Site,
      range:    PlotRange,
      time:     TimeDisplay
    )(using TransactionalClient[F, UserPreferencesDB]): F[Unit] =
      import UserTargetPreferencesUpsert.*
      execute[F](
        LucumaTargetInsertInput(
          target_id = targetId.show.assign,
          lucuma_elevation_plot_preferences = LucumaElevationPlotPreferencesArrRelInsertInput(
            data = List(
              LucumaElevationPlotPreferencesInsertInput(
                user_id = uid.show.assign,
                site = site.assign,
                range = range.assign,
                time = time.assign
              )
            ),
            on_conflict = LucumaElevationPlotPreferencesOnConflict(
              constraint =
                LucumaElevationPlotPreferencesConstraint.LucumaElevationPlotPreferencesPkey,
              update_columns = List(LucumaElevationPlotPreferencesUpdateColumn.Site,
                                    LucumaElevationPlotPreferencesUpdateColumn.Range,
                                    LucumaElevationPlotPreferencesUpdateColumn.Time
              )
            ).assign
          ).assign
        )
      ).attempt.void

    // Gets the prefs for the elevation plot
    def queryWithDefault[F[_]: ApplicativeThrow](
      uid:         User.Id,
      tid:         Target.Id,
      defaultSite: Site
    )(using TransactionalClient[F, UserPreferencesDB]): F[(Site, PlotRange, TimeDisplay)] =
      import UserElevationPlotPreferencesQuery.*
      for r <-
          query[F](uid.show, tid.show)
            .map { r =>
              r.lucuma_elevation_plot_preferences_by_pk.map(result =>
                (result.site, result.range, result.time)
              )
            }
            .handleError(_ => none)
      yield
        val site  = r.map(_._1).getOrElse(defaultSite)
        val range = r.map(_._2).getOrElse(PlotRange.Night)
        val time  = r.map(_._3).getOrElse(TimeDisplay.Site)

        (site, range, time)

  implicit class UserTargetPreferencesUpdateOps(
    val self: UserTargetPreferencesFovUpdate.type
  ) extends AnyVal {
    import self.*

    def updateViewOffset[F[_]: ApplicativeThrow](
      uid:      User.Id,
      targetId: Target.Id,
      offset:   Offset
    )(implicit
      cl:       TransactionalClient[F, UserPreferencesDB]
    ): F[Unit] =
      execute[F](
        user_id = uid.show,
        target_id = targetId.show,
        viewOffsetP = offset.p.toAngle.toMicroarcseconds,
        viewOffsetQ = offset.q.toAngle.toMicroarcseconds
      ).attempt.void
  }
}
