package explore.graphql

import io.circe.generic.extras._

package object client {
    protected[client] type MessageType = String

    private val messageTypes: Map[String, String] = Map(
            "ConnectionInit" -> "connection_init",
            "ConnectionAck" -> "connection_ack",
            "KeepAlive" -> "ka",
            "Start" -> "start",
            "Stop" -> "stop",
            "Data" -> "data"
        )

    implicit protected[client] val genDevConfig: Configuration =
        Configuration.default.withDiscriminator("type")
            .copy(transformConstructorNames = messageTypes)
}
