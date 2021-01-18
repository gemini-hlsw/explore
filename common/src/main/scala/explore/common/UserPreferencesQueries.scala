// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.target

import clue.GraphQLOperation
import clue.macros.GraphQL
import explore.GraphQLSchemas.UserPreferencesDB
import lucuma.core.model.User
import lucuma.core.util.Gid
import explore.model.ResizableSection
import cats.syntax.all._
import clue.GraphQLClient
import cats.data.OptionT
import cats.MonadError

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
              query[F](Gid[User.Id].show(uid), area.value)
                .map { r =>
                  r.explore_resizable_width_by_pk.flatMap(_.width)
                }
                .recover { e => println(e); none }
            }
      } yield w).value.map(_.flatten.getOrElse(defaultValue))
  }
}
