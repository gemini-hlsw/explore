// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import cats.MonadError
import cats.data.OptionT
import cats.effect.Effect
import cats.syntax.all._
import clue.macros.GraphQL
import clue.{ GraphQLClient, GraphQLOperation }
import clue.data.syntax._
import explore.GraphQLSchemas.UserPreferencesDB
import explore.model.ResizableSection
import explore.model.GridLayoutSection
import explore.model.layout._
import lucuma.core.model.User
import lucuma.core.model.Target
import react.gridlayout.{ BreakpointName => _, _ }
import scala.collection.immutable.SortedMap
import lucuma.core.math.Angle

object UserPreferencesQueries {

  /**
   * Query to create a user, this is called when the app is started.
   * If the user exists the error is ignored
   */
  @GraphQL
  object UserInsertMutation extends GraphQLOperation[UserPreferencesDB] {
    val document = """
      mutation insert_user($id: String) {
        insert_lucuma_user_one(
          object: {
            user_id: $id
          },
          on_conflict: {
            update_columns: [],
            constraint: lucuma_user_pkey
          }
        ) {
          user_id
          }
        }
    """
  }

  /**
   * Update the width of a given area/user
   */
  @GraphQL
  object UserWidthsCreation extends GraphQLOperation[UserPreferencesDB] {
    val document = """
      mutation update_area_width($item: explore_resizable_width_insert_input!) {
        insert_explore_resizable_width_one(
          object: $item,
          on_conflict: {
            constraint: explore_resizable_width_pkey,
            update_columns: width
          }
        ) {
          user_id
          }
        }
   """

    def storeWidthPreference[F[_]: Effect](
      userId:  Option[User.Id],
      section: ResizableSection,
      width:   Int
    )(implicit
      cl:      GraphQLClient[F, UserPreferencesDB]
    ): F[Unit] =
      userId.traverse { i =>
        UserWidthsCreation
          .execute[F](
            WidthUpsertInput(i, section, width)
          )
          .attempt
      }.void

  }

  /**
   * Read the stored width of an area
   */
  @GraphQL(debug = false)
  object UserAreaWidths extends GraphQLOperation[UserPreferencesDB] {
    val document = """
      query area_width($user_id: String!, $section: resizable_area!) {
        explore_resizable_width_by_pk(
          section: $section,
          user_id: $user_id
        ) {
          width
        }
      }
    """

    // Gets the width of a section.
    // This is coded to return a default in case
    // there is no data or errors
    def queryWithDefault[F[_]: MonadError[*[_], Throwable]](
      userId:       Option[User.Id],
      area:         ResizableSection,
      defaultValue: Int
    )(implicit cl:  GraphQLClient[F, UserPreferencesDB]): F[Int] =
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
  }

  /**
   * Read the grid layout for a given section
   */
  @GraphQL(debug = false)
  object UserGridLayoutQuery extends GraphQLOperation[UserPreferencesDB] {
    val document = """
      query layout_positions($criteria: grid_layout_positions_bool_exp!) {
        grid_layout_positions(where: $criteria) {
          breakpoint_name
          height
          width
          x
          y
          tile
        }
      }"""

    def positions2LayoutMap(
      g: (BreakpointName, List[Data.GridLayoutPositions])
    ): (react.gridlayout.BreakpointName, (Int, Int, Layout)) = {
      val bn = breakpointNameFromString(g._1)
      bn -> ((breakpointWidth(bn),
              breakpointCols(bn),
              Layout(
                g._2.map(p => LayoutItem(p.width, p.height, p.x, p.y, p.tile))
              )
             )
      )
    }

    // Gets the layout of a section.
    // This is coded to return a default in case
    // there is no data or errors
    def queryWithDefault[F[_]: MonadError[*[_], Throwable]](
      userId:       Option[User.Id],
      area:         GridLayoutSection,
      defaultValue: LayoutsMap
    )(implicit cl:  GraphQLClient[F, UserPreferencesDB]): F[LayoutsMap] =
      (for {
        uid <- OptionT.fromOption[F](userId)
        c   <-
          OptionT.pure(
            GridLayoutPositionsBoolExp(user_id = StringComparisonExp(uid.show.assign).assign)
          )
        r   <-
          OptionT
            .liftF[F, SortedMap[react.gridlayout.BreakpointName, (Int, Int, Layout)]] {
              query[F](c).map { r =>
                if (r.grid_layout_positions.isEmpty) defaultValue
                else
                  SortedMap(
                    r.grid_layout_positions
                      .groupBy(_.breakpoint_name)
                      .map(positions2LayoutMap)
                      .toList: _*
                  )
              }
            }
            .handleErrorWith(_ => OptionT.none)
      } yield r).getOrElse(defaultValue)
  }

  @GraphQL(debug = false)
  object UserGridLayoutUpsert extends GraphQLOperation[UserPreferencesDB] {
    val document = """
      mutation insert_layout_positions($objects: [grid_layout_positions_insert_input!]! = {}) {
        insert_grid_layout_positions(objects: $objects, on_conflict: {
          constraint: grid_layout_positions_pkey,
          update_columns: [width, height, x, y]
        }) {
          affected_rows
        }
      }"""

    def storeLayoutsPreference[F[_]: Effect](
      userId:  Option[User.Id],
      section: GridLayoutSection,
      layouts: Layouts
    )(implicit
      cl:      GraphQLClient[F, UserPreferencesDB]
    ): F[Unit] =
      userId.traverse { uid =>
        UserGridLayoutUpsert
          .execute[F](
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
          )
          .attempt
      }.void

  }
  @GraphQL(debug = false)
  object UserTargetPreferencesQuery extends GraphQLOperation[UserPreferencesDB] {
    val document = """
      query target_preferences($user_id: String! = "", $target_id: String! = "") {
        lucuma_target_preferences_by_pk(target_id: $target_id, user_id: $user_id) {
          fov
        }
      }
    """

    // Gets the layout of a section.
    // This is coded to return a default in case
    // there is no data or errors
    def queryWithDefault[F[_]: MonadError[*[_], Throwable]](
      uid:         User.Id,
      tid:         Target.Id,
      defaultFov:  Angle
    )(implicit cl: GraphQLClient[F, UserPreferencesDB]): F[Angle] =
      (for {
        r <-
          query[F](uid.show, tid.show)
            .map { r =>
              r.lucuma_target_preferences_by_pk.map(_.fov)
            }
            .handleError(_ => none)
      } yield r.map(Angle.fromMicroarcseconds)).map(_.getOrElse(defaultFov))
  }

  @GraphQL(debug = false)
  object UserTargetPreferencesUpsert extends GraphQLOperation[UserPreferencesDB] {
    val document =
      """mutation target_preferences_upsert($objects: lucuma_target_insert_input! = {}) {
        insert_lucuma_target(objects: [$objects], on_conflict: {constraint: lucuma_target_pkey, update_columns: target_id}) {
          affected_rows
        }
      }"""

    def updateFov[F[_]: Effect](
      uid:      User.Id,
      targetId: Target.Id,
      fov:      Angle
    )(implicit
      cl:       GraphQLClient[F, UserPreferencesDB]
    ): F[Unit] =
      UserTargetPreferencesUpsert
        .execute[F](
          LucumaTargetInsertInput(
            target_id = targetId.show.assign,
            lucuma_target_preferences = LucumaTargetPreferencesArrRelInsertInput(
              data = List(
                LucumaTargetPreferencesInsertInput(user_id = uid.show.assign,
                                                   fov = fov.toMicroarcseconds.assign
                )
              ),
              on_conflict = LucumaTargetPreferencesOnConflict(
                constraint = LucumaTargetPreferencesConstraint.LucumaTargetPreferencesPkey,
                update_columns = List(LucumaTargetPreferencesUpdateColumn.Fov)
              ).assign
            ).assign
          )
        )
        .attempt
        .void
  }

}
