// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

//import crystal._
import explore.graphql.TestQuery
import cats.effect.Async
import cats.implicits._
import clue._

trait PersonsActions[F[_]] {
  def query(): F[List[TestQuery.AllPersons]]
//   def set(newPersons: List[TestQuery.AllPersons]): F[Unit]
}

class PersonsActionsInterpreter[F[_]: Async](swClient: GraphQLClient[F]) /*(lens: FixedLens[F, List[TestQuery.AllPersons]])*/
    extends PersonsActions[F] {
  override def query(): F[List[TestQuery.AllPersons]] = {
    val result = swClient.query(TestQuery)()
    result.map(_.allPersons)
  }

//   override def set(newPersons: List[TestQuery.AllPersons]): F[Unit] = lens.set(newPersons)
}
