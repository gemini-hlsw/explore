package explore.graphql.client

import cats.effect._
import cats.implicits._
import io.circe._
import io.circe.syntax._

// Effects are purposely declared in individual methods instead of the trait.
// This is so that the methods can be easily called from tagless code.
trait GraphQLClient {
    val uri: String

    def query[F[_] : Async](graphQLQuery: GraphQLQuery)(variables: Option[graphQLQuery.Variables] = None): F[graphQLQuery.Data] = {
        import graphQLQuery._
        
        variables.fold(query[F, graphQLQuery.Data](graphQLQuery.document)){v => 
            query[F, graphQLQuery.Variables, graphQLQuery.Data](graphQLQuery.document, v)}
    }

    def query[F[_] : Async, V : Encoder, D : Decoder](document: String, variables: V, operationName: String): F[D] = {
        queryInternal[F, V, D](document, operationName.some, variables.asJson.some) 
    }

    def query[F[_] : Async,  D: Decoder](document: String, operationName: String): F[D] = {
        queryInternal[F, Nothing, D](document, operationName.some)
    }

    def query[F[_] : Async, V : Encoder, D : Decoder](document: String, variables: V): F[D] = {
        queryInternal[F, V, D](document, None, variables.asJson.some) 
    }

    def query[F[_] : Async, D: Decoder](document: String): F[D] = {
        queryInternal[F, Nothing, D](document)
    }

    protected def queryInternal[F[_] : Async, V, D: Decoder](document: String, operationName: Option[String] = None, variables: Option[Json] = None): F[D]
}
