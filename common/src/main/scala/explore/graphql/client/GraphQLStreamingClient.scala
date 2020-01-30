package explore.graphql.client

import cats.implicits._
import io.circe._
import io.circe.syntax._

// Effects are purposely declared in individual methods instead of the trait.
// This is so that the methods can be easily called from tagless code.
trait GraphQLStreamingClient[E[_[_]]] extends GraphQLClient[E] {
    val uri: String

    protected trait Stoppable[F[_]] {
        def stop: F[Unit]
    }

    type Subscription[F[_], D] <: Stoppable[F]

    def subscribe[F[_] : E](graphQLQuery: GraphQLQuery)(variables: Option[graphQLQuery.Variables] = None): F[Subscription[F, graphQLQuery.Data]] = {
        import graphQLQuery._
        
        variables.fold(subscribe[F, graphQLQuery.Data](graphQLQuery.document)){v => 
            subscribe[F, graphQLQuery.Variables, graphQLQuery.Data](graphQLQuery.document, v)}
    }

    def subscribe[F[_] : E, V : Encoder, D : Decoder](subscription: String, variables: V, operationName: String): F[Subscription[F, D]] = {
        subscribeInternal[F, D](subscription, operationName.some, variables.asJson.some) 
    }

    def subscribe[F[_] : E,  D: Decoder](subscription: String, operationName: String): F[Subscription[F, D]] = {
        subscribeInternal[F, D](subscription, operationName.some)
    }

    def subscribe[F[_] : E, V : Encoder, D : Decoder](subscription: String, variables: V): F[Subscription[F, D]] = {
        subscribeInternal[F, D](subscription, None, variables.asJson.some) 
    }

    def subscribe[F[_] : E, D: Decoder](subscription: String): F[Subscription[F, D]] = {
        subscribeInternal[F, D](subscription)
    }

    protected def subscribeInternal[F[_] : E, D : Decoder](subscription: String, operationName: Option[String] = None, variables: Option[Json] = None): F[Subscription[F, D]]
}