package explore.graphql.client

trait GraphQLClient[F[_]] {
  val uri: String
  def query(query: GraphQLQuery)(variables: Option[query.Variables]): F[query.Data]
}
