// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import cats.ApplicativeThrow
import cats.MonadThrow
import cats.Order.*
import cats.data.NonEmptyList
import cats.data.OptionT
import cats.syntax.all.*
import clue.FetchClient
import clue.data.syntax.*
import clue.syntax.*
import eu.timepit.refined.*
import eu.timepit.refined.numeric.*
import explore.givens.given
import explore.model.AladinFullScreen
import explore.model.AladinMouseScroll
import explore.model.AsterismVisualOptions
import explore.model.Attachment
import explore.model.ChartOp
import explore.model.ColorsInverted
import explore.model.GlobalPreferences
import explore.model.Observation
import explore.model.Transformation
import explore.model.enums.GridBreakpointName
import explore.model.enums.GridLayoutSection
import explore.model.enums.PlotRange
import explore.model.enums.TableId
import explore.model.enums.TimeDisplay
import explore.model.enums.Visible
import explore.model.enums.WavelengthUnits
import explore.model.itc.*
import explore.model.layout.*
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.util.Enumerated
import lucuma.itc.GraphType
import lucuma.react.gridlayout.*
import lucuma.react.table.*
import lucuma.ui.table.hooks.*
import org.typelevel.log4cats.Logger
import queries.common.UserPreferencesQueriesGQL.*
import queries.common.UserPreferencesQueriesGQL.AsterismPreferencesQuery.Data.ExploreAsterismPreferences
import queries.common.UserPreferencesQueriesGQL.UserGridLayoutUpdates.Data.LucumaGridLayoutPositions
import queries.schemas.UserPreferencesDB
import queries.schemas.UserPreferencesDB.Enums.*
import queries.schemas.UserPreferencesDB.Types.*

import scala.collection.immutable.SortedMap

object UserPreferencesQueries:
  type TableColumnPreferences = TableColumnPreferencesQuery.Data
  val TableColumnPreferences = TableColumnPreferencesQuery.Data

  object GlobalUserPreferences:
    def loadPreferences[F[_]: MonadThrow](
      userId: User.Id
    )(using FetchClient[F, UserPreferencesDB]): F[GlobalPreferences] =
      UserPreferencesQuery[F]
        .query(userId.show)
        .raiseGraphQLErrors
        .map(_.lucumaUserPreferencesByPk)
        .handleError(_ => none)
        .map(_.getOrElse(GlobalPreferences.Default))

    // We could pass the full prefs but this is more efficient
    def storeAladinPreferences[F[_]: ApplicativeThrow](
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
          ),
          update_columns = List(
            LucumaUserPreferencesUpdateColumn.AladinMouseScroll.some
              .filter(_ => aladinMouseScroll.isDefined),
            LucumaUserPreferencesUpdateColumn.ShowCatalog.some.filter(_ => showCatalog.isDefined),
            LucumaUserPreferencesUpdateColumn.AgsOverlay.some.filter(_ => agsOverlay.isDefined),
            LucumaUserPreferencesUpdateColumn.ScienceOffsets.some.filter(_ =>
              scienceOffsets.isDefined
            ),
            LucumaUserPreferencesUpdateColumn.AcquisitionOffsets.some.filter(_ =>
              acquisitionOffsets.isDefined
            ),
            LucumaUserPreferencesUpdateColumn.FullScreen.some.filter(_ => fullScreen.isDefined)
          ).flattenOption.widen[LucumaUserPreferencesUpdateColumn].assign
        )
        .attempt
        .void

  end GlobalUserPreferences

  object GridLayouts:
    extension (e: BreakpointName)
      def toGridBreakpointName: GridBreakpointName =
        Enumerated[GridBreakpointName].unsafeFromTag(e.name)

    private trait DBLayoutPosition[A]:
      extension (a: A)
        def breapointName: BreakpointName
        def layoutItem: LayoutItem

    private given dataPos: DBLayoutPosition[UserGridLayoutQuery.Data.LucumaGridLayoutPositions] with
      extension (a: UserGridLayoutQuery.Data.LucumaGridLayoutPositions)
        def breapointName: BreakpointName = breakpointNameFromString(a.breakpointName.tag)
        def layoutItem: LayoutItem        = LayoutItem(a.width, a.height, a.x, a.y, a.tile)

    private given subsPos: DBLayoutPosition[UserGridLayoutUpdates.Data.LucumaGridLayoutPositions]
    with
      extension (a: UserGridLayoutUpdates.Data.LucumaGridLayoutPositions)
        def breapointName: BreakpointName = breakpointNameFromString(a.breakpointName.tag)
        def layoutItem: LayoutItem        = LayoutItem(a.width, a.height, a.x, a.y, a.tile)

    private def positions2LayoutMap[A](
      g: (GridBreakpointName, List[A])
    )(using dbPos: DBLayoutPosition[A]): (BreakpointName, (Int, Int, Layout)) =
      val bn = breakpointNameFromString(g._1.tag)
      bn -> ((breakpointWidth(bn),
              breakpointCols(bn),
              Layout(
                g._2.map(_.layoutItem)
              )
      ))

    // Gets the layouts for all of the sections.
    def queryLayouts[F[_]: MonadThrow](
      userId: Option[User.Id]
    )(using FetchClient[F, UserPreferencesDB]): F[Option[Map[GridLayoutSection, LayoutsMap]]] =
      (for {
        uid <- OptionT.fromOption[F](userId)
        r   <-
          OptionT
            .liftF[F, Map[GridLayoutSection, SortedMap[BreakpointName, (Int, Int, Layout)]]] {
              UserGridLayoutQuery[F]
                .query(uid.show)
                .raiseGraphQLErrors
                .map: r =>
                  r.lucumaGridLayoutPositions match {
                    case l if l.isEmpty => Map.empty
                    case l              =>
                      l.groupBy(_.section).map { case (s, l) =>
                        s -> SortedMap.from(
                          l.groupBy(_.breakpointName).map(positions2LayoutMap)
                        )
                      }
                  }
            }
            .handleErrorWith(_ => OptionT.none)
      } yield r).value

    def updateLayouts(
      data: List[LucumaGridLayoutPositions]
    ): Map[GridLayoutSection, LayoutsMap] => Map[GridLayoutSection, LayoutsMap] = original =>
      data match {
        case l if l.isEmpty => original
        case l              =>
          val newMap = l.groupBy(_.section).map { case (s, l) =>
            s -> SortedMap.from(
              l.groupBy(_.breakpointName).map(positions2LayoutMap)
            )
          }
          mergeSectionLayoutsMaps(original, newMap)
      }

    def storeLayoutsPreference[F[_]: ApplicativeThrow](
      userId:  Option[User.Id],
      section: GridLayoutSection,
      layouts: Layouts
    )(using FetchClient[F, UserPreferencesDB]): F[Unit] =
      userId.traverse { uid =>
        UserGridLayoutUpsert[F]
          .execute(
            layouts.layouts.flatMap { bl =>
              bl.layout.asList.collect {
                case i if i.i.nonEmpty && i.h > 0 =>
                  LucumaGridLayoutPositionsInsertInput(
                    userId = uid.show.assign,
                    section = section.assign,
                    breakpointName = bl.name.toGridBreakpointName.assign,
                    width = i.w.assign,
                    height = i.h.assign,
                    x = i.x.assign,
                    y = i.y.assign,
                    tile = i.i.assign
                  )
              }
            }
          )
          .attempt
      }.void

    def deleteLayoutsPreference[F[_]: ApplicativeThrow](userId: User.Id)(using
      FetchClient[F, UserPreferencesDB]
    ): F[Unit] =
      UserGridLayoutsDelete[F].execute(userId.show).void

  end GridLayouts

  object AsterismPreferences:
    // Gets the asterism properties, if not foun use the default
    def queryWithDefault[F[_]: MonadThrow](
      uid:        User.Id,
      tids:       NonEmptyList[Target.Id],
      defaultFov: Angle
    )(using
      FetchClient[F, UserPreferencesDB]
    ): F[AsterismVisualOptions] = {
      val targetIds = tids.toList.map(_.show)
      AsterismPreferencesQuery[F]
        .query(uid.show, targetIds.assign)
        .raiseGraphQLErrors
        .flatMap { r =>
          val asterismPrefsResult = r.exploreAsterismPreferences

          asterismPrefsResult
            .filter {
              // Unfortunately we can't use the DB to filter by targetId,
              // instead we have to do it here by hand
              _.lucumaAsterisms.map(_.targetId).forall(i => targetIds.exists(_ === i))
            }
            .headOption
            .map { result =>

              val fovRA =
                result.fovRA
                  .map(Angle.fromMicroarcseconds)
                  .getOrElse(defaultFov)

              val fovDec =
                result.fovDec
                  .map(Angle.fromMicroarcseconds)
                  .getOrElse(defaultFov)

              val offset =
                (result.viewOffsetP.map(Angle.fromMicroarcseconds(_).p),
                 result.viewOffsetQ.map(Angle.fromMicroarcseconds(_).q)
                )
                  .mapN(Offset.apply)
                  .getOrElse(Offset.Zero)

              def rangeProp(op: ExploreAsterismPreferences => Option[Int]) =
                op(result)
                  .flatMap(refineV[Interval.Closed[0, 100]](_).toOption)
                  .getOrElse(AsterismVisualOptions.Default.saturation)

              val saturation = rangeProp(_.saturation)
              val brightness = rangeProp(_.brightness)

              AsterismVisualOptions(result.id.some, fovRA, fovDec, offset, saturation, brightness)
                .pure[F]
            }
            .getOrElse {
              updateAladinPreferences[F](
                None,
                uid,
                tids,
                fovRA = defaultFov.some,
                fovDec = defaultFov.some
              ).map(i => AsterismVisualOptions.Default.copy(i))
            }
        }
        .handleError { e =>
          AsterismVisualOptions.Default
        }
    }

    def updateAladinPreferences[F[_]: MonadThrow](
      prefsId:    Option[Int],
      uid:        User.Id,
      targetId:   NonEmptyList[Target.Id],
      fovRA:      Option[Angle] = None,
      fovDec:     Option[Angle] = None,
      saturation: Option[Int] = None,
      brightness: Option[Int] = None,
      offset:     Option[Offset] = None
    )(using FetchClient[F, UserPreferencesDB]): F[Option[Int]] =
      AsterismUpsert[F]
        .execute(
          ExploreAsterismPreferencesInsertInput(
            id = prefsId.orIgnore,
            userId = uid.show.assign,
            brightness = brightness.orIgnore,
            saturation = saturation.orIgnore,
            fovRA = fovRA.map(_.toMicroarcseconds).orIgnore,
            fovDec = fovDec.map(_.toMicroarcseconds).orIgnore,
            viewOffsetP = offset.map(_.p.toAngle.toMicroarcseconds).orIgnore,
            viewOffsetQ = offset.map(_.q.toAngle.toMicroarcseconds).orIgnore,
            lucumaAsterisms = LucumaAsterismArrRelInsertInput(
              data =
                targetId.map(t => LucumaAsterismInsertInput(targetId = t.toString.assign)).toList,
              onConflict = LucumaAsterismOnConflict(
                constraint = LucumaAsterismConstraint.LucumaAsterismPkey,
                update_columns = List(
                  LucumaAsterismUpdateColumn.TargetId.some
                ).flattenOption
              ).assign
            ).assign
          ),
          updateColumns = List(
            ExploreAsterismPreferencesUpdateColumn.FovRA.some.filter(_ => fovRA.isDefined),
            ExploreAsterismPreferencesUpdateColumn.FovDec.some.filter(_ => fovDec.isDefined),
            ExploreAsterismPreferencesUpdateColumn.Saturation.some.filter(_ =>
              saturation.isDefined
            ),
            ExploreAsterismPreferencesUpdateColumn.Brightness.some.filter(_ =>
              brightness.isDefined
            ),
            ExploreAsterismPreferencesUpdateColumn.ViewOffsetP.some.filter(_ => offset.isDefined),
            ExploreAsterismPreferencesUpdateColumn.ViewOffsetQ.some.filter(_ => offset.isDefined)
          ).flattenOption.assign
        )
        .raiseGraphQLErrors
        .map(_.insertExploreAsterismPreferencesOne.map(_.id))
        .attempt
        .map(_.getOrElse(None))

  object FinderChartPreferences:
    // Gets the prefs for the itc plot
    def queryWithDefault[F[_]: MonadThrow](
      oid: Observation.Id,
      aid: Attachment.Id
    )(using FetchClient[F, UserPreferencesDB]): F[Transformation] =
      FinderChartTransformationQuery[F]
        .query(aid.show, oid.show)
        .raiseGraphQLErrors
        .map: r =>
          r.exploreFinderChartByPk.map: r =>
            Transformation(
              ChartOp.Rotate(r.rotate),
              ChartOp.ScaleX(r.scaleX.toDouble / 100),
              ChartOp.ScaleY(r.scaleY.toDouble / 100),
              ChartOp.FlipX(r.flipX),
              ChartOp.FlipY(r.flipY),
              ColorsInverted(r.inverted)
            )
        .handleError(_ => none)
        .map(_.getOrElse(Transformation.Default))

    def updateTransformation[F[_]: ApplicativeThrow](
      oid:       Observation.Id,
      aid:       Attachment.Id,
      transform: Transformation
    )(using FetchClient[F, UserPreferencesDB]): F[Unit] =
      FinderChartUpsert[F]
        .execute(
          observationId = oid.show,
          ExploreFinderChartInsertInput(
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
    def updatePlotPreferences[F[_]: ApplicativeThrow](
      userId:         User.Id,
      itcChartType:   GraphType,
      itcDetailsOpen: PlotDetails
    )(using FetchClient[F, UserPreferencesDB]): F[Unit] =
      UserPreferencesItcPlotUpdate[F]
        .execute(
          userId = userId.show.assign,
          itcChartType = itcChartType,
          itcDetailsOpen = itcDetailsOpen.value
        )
        .attempt
        .void
  end ItcPlotPreferences

  object WavelengthUnitsPreference:
    def updateWavelengthUnits[F[_]: ApplicativeThrow](
      userId: User.Id,
      units:  WavelengthUnits
    )(using FetchClient[F, UserPreferencesDB]): F[Unit] =
      UserWavelengthUnitsUpdate[F]
        .execute(
          userId = userId.show.assign,
          wavelengthUnits = units.assign
        )
        .attempt
        .void
  end WavelengthUnitsPreference

  object ElevationPlotPreference:
    def updatePlotPreferences[F[_]: ApplicativeThrow](
      userId:                               User.Id,
      range:                                PlotRange,
      time:                                 TimeDisplay,
      scheduling:                           Boolean,
      elevationPlotElevationVisible:        Visible,
      elevationPlotParallacticAngleVisible: Visible,
      elevationPlotSkyBrightnessVisible:    Visible,
      elevationPlotLunarElevationVisible:   Visible
    )(using FetchClient[F, UserPreferencesDB]): F[Unit] =
      UserPreferencesElevPlotUpdate[F]
        .execute(
          userId = userId.show.assign,
          elevationPlotRange = range,
          elevationPlotTime = time,
          elevationPlotScheduling = scheduling,
          elevationPlotElevationVisible = elevationPlotElevationVisible.isVisible,
          elevationPlotParallacticAngleVisible = elevationPlotParallacticAngleVisible.isVisible,
          elevationPlotSkyBrightnessVisible = elevationPlotSkyBrightnessVisible.isVisible,
          elevationPlotLunarElevationVisible = elevationPlotLunarElevationVisible.isVisible
        )
        .attempt
        .void
  end ElevationPlotPreference

  case class TableStore[F[_]: MonadThrow, TF](
    userId:                Option[User.Id],
    tableId:               TableId,
    columns:               List[ColumnDef[?, ?, ?, ?, TF, ?, ?]],
    // Allow for the ignoring of visibility while still including sort order.
    excludeFromVisibility: Set[ColumnId] = Set.empty
  )(using FetchClient[F, UserPreferencesDB], Logger[F])
      extends TableStateStore[F, TF]:
    def load(): F[TableState[TF] => TableState[TF]] =
      userId
        .traverse: uid =>
          TableColumnPreferencesQuery[F]
            .query(
              userId = uid.show.assign,
              tableId = tableId.assign
            )
            .raiseGraphQLErrors
            .recoverWith: t =>
              Logger[F]
                .error(t)(s"Error loading table preferences for [$tableId]")
                .as(TableColumnPreferencesQuery.Data(Nil))
            .map: prefs =>
              (tableState: TableState[TF]) =>
                tableState
                  .setColumnVisibility:
                    prefs.lucumaTableColumnPreferences.applyVisibility(
                      tableState.columnVisibility,
                      excludeFromVisibility
                    )
                  .setSorting(prefs.lucumaTableColumnPreferences.applySorting(tableState.sorting))
        .map(_.orEmpty)

    def save(state: TableState[TF]): F[Unit] =
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
    def applyVisibility(original: ColumnVisibility, exclude: Set[ColumnId]): ColumnVisibility =
      original.modify(
        _ ++
          tableColsPrefs
            .filterNot(col => exclude.contains(ColumnId(col.columnId)))
            .map(col => ColumnId(col.columnId) -> Visibility.fromVisible(col.visible))
      )

    def applySorting(original: Sorting): Sorting =
      val sortedCols =
        tableColsPrefs
          .flatMap(col => (ColumnId(col.columnId).some, col.sorting, col.sortingOrder).tupled)
          .sortBy(_._3)

      // We don't force unsorting, in case there's a default sorting.
      sortedCols match
        case Nil      => original
        case nonEmpty => Sorting(nonEmpty.map((colId, dir, _) => colId -> dir)*)
