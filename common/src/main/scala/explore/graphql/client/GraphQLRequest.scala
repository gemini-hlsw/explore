package explore.graphql.client

import io.circe.Json
import io.circe.generic.JsonCodec

// Request
// {
//   "query": "...",
//   "operationName": "...",
//   "variables": { "myVariable": "someValue", ... }
// }

@JsonCodec
protected[client] final case class GraphQLRequest(
    query: String,
    operationName: Option[String] = None,
    variables: Option[Json] = None
)