package explore.graphql.client

import io.circe.generic.extras._
import io.circe.Json
import io.circe.generic.JsonCodec

@ConfiguredJsonCodec
protected[client] sealed trait StreamingMessage

protected[client] sealed trait PayloadMessage[P] extends StreamingMessage {
    val payload: P
}

protected[client] final case class ConnectionInit(payload: Map[String, String] = Map.empty)
        extends PayloadMessage[Map[String, String]]

protected[client] case object ConnectionAck extends StreamingMessage

protected[client] case object KeepAlive extends StreamingMessage

protected[client] sealed trait SubscriptionMessage extends StreamingMessage {
    val id: String
}

protected[client] final case class Start(id: String, payload: GraphQLRequest) 
    extends SubscriptionMessage with PayloadMessage[GraphQLRequest]

protected[client] final case class Stop(id: String) extends SubscriptionMessage

@JsonCodec
protected[client] final case class DataWrapper(data: Json)

protected[client] final case class Data(id: String, payload: DataWrapper)
    extends SubscriptionMessage with PayloadMessage[DataWrapper]

protected[client] object DataJson {
    def unapply(data: Data): Option[(String, Json)] = Some((data.id, data.payload.data))
}
