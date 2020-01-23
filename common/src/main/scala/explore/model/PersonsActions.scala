package explore.model

//import crystal._
import explore.graphql.TestQuery
import cats.effect.LiftIO

trait PersonsActions[F[_]] {
  def query(): F[List[TestQuery.AllPersons]]  
//   def set(newPersons: List[TestQuery.AllPersons]): F[Unit]
}

class PersonsActionsInterpreter[F[_] : LiftIO]/*(lens: FixedLens[F, List[TestQuery.AllPersons]])*/ extends PersonsActions[F] {
  override def query(): F[List[TestQuery.AllPersons]] =
    LiftIO[F].liftIO(AppState.swapiClient.query(TestQuery)().map(_.allPersons))

//   override def set(newPersons: List[TestQuery.AllPersons]): F[Unit] = lens.set(newPersons)
}