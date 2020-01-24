package explore.model

//import crystal._
import explore.graphql.TestQuery
import cats.effect.LiftIO
import cats.Functor
import cats.implicits._

trait PersonsActions[F[_]] {
  def query(): F[List[TestQuery.AllPersons]]  
//   def set(newPersons: List[TestQuery.AllPersons]): F[Unit]
}

class PersonsActionsInterpreter[F[_] : LiftIO : Functor]/*(lens: FixedLens[F, List[TestQuery.AllPersons]])*/ extends PersonsActions[F] {
  override def query(): F[List[TestQuery.AllPersons]] = {
    val result = AppState.swapiClient.query[F](TestQuery)()
    result.map(_.allPersons)
  }

//   override def set(newPersons: List[TestQuery.AllPersons]): F[Unit] = lens.set(newPersons)
}