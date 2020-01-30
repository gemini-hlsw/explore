package explore.graphql.client

import cats.implicits._
import io.circe._
import io.circe.syntax._

// Effects are purposely declared in individual methods instead of the trait.
// This is so that the methods can be easily called from tagless code.
trait GraphQLClient[E[_[_]]] {
    val uri: String

    def query[F[_] : E](graphQLQuery: GraphQLQuery)(variables: Option[graphQLQuery.Variables] = None): F[graphQLQuery.Data] = {
        import graphQLQuery._
        
        variables.fold(query[F, graphQLQuery.Data](graphQLQuery.document)){v => 
            query[F, graphQLQuery.Variables, graphQLQuery.Data](graphQLQuery.document, v)}
    }

    def query[F[_] : E, V : Encoder, D : Decoder](document: String, variables: V, operationName: String): F[D] = {
        queryInternal[F, D](document, operationName.some, variables.asJson.some) 
    }

    def query[F[_] : E,  D: Decoder](document: String, operationName: String): F[D] = {
        queryInternal[F, D](document, operationName.some)
    }

    def query[F[_] : E, V : Encoder, D : Decoder](document: String, variables: V): F[D] = {
        queryInternal[F, D](document, None, variables.asJson.some) 
    }

    def query[F[_] : E, D: Decoder](document: String): F[D] = {
        queryInternal[F, D](document)
    }

    protected def queryInternal[F[_] : E, D: Decoder](document: String, operationName: Option[String] = None, variables: Option[Json] = None): F[D]
}
