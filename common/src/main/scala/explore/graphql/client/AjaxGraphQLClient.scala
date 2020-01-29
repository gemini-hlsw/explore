package explore.graphql.client

import cats.effect._
import org.scalajs.dom.ext.Ajax
import io.circe._
import io.circe.syntax._
import io.circe.parser._

import scala.concurrent.ExecutionContext.Implicits._
import scala.util.Success
import scala.util.Failure

case class AjaxGraphQLClient(uri: String) extends GraphQLClient {
    // Response
    // {
    //   "data": { ... }, // Typed
    //   "errors": [ ... ]
    // }

    protected def queryInternal[F[_] : Async, V, D: Decoder](document: String, operationName: Option[String] = None, variables: Option[Json] = None): F[D] = 
        Async[F].async{ cb =>
            Ajax.post(
                url = uri,
                data = GraphQLRequest(document, operationName = operationName, variables = variables).asJson.toString,
                headers = Map("Content-Type" -> "application/json")
            ).onComplete{
                case Success(r) =>
                    val data = parse(r.responseText).flatMap{ json =>
                        val cursor = json.hcursor
                        cursor.get[List[Json]]("errors")
                            .map(errors => new GraphQLException(errors))
                            .swap
                            .flatMap(_ => cursor.get[D]("data"))
                    }
                    cb(data)
                case Failure(t) =>
                    cb(Left(t))
            }
        }
}
