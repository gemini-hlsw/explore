package explore.graphql.client

import fs2.Stream
import cats.effect._
import cats.implicits._
import org.scalajs.dom.raw.WebSocket
import io.circe._
import io.circe.syntax._
import io.circe.parser._
import fs2.concurrent.Queue
import java.util.UUID
import scala.collection.mutable
import scala.scalajs.js
import org.scalajs.dom.raw.Event
import org.scalajs.dom.raw.MessageEvent
import cats.effect.concurrent.Deferred
import cats.data.EitherT

// This implementation follows the Apollo protocol, specified in:
// https://github.com/apollographql/subscriptions-transport-ws/blob/master/PROTOCOL.md
// Also see: https://medium.com/@rob.blackbourn/writing-a-graphql-websocket-subscriber-in-javascript-4451abb9cd60
case class WebSocketGraphQLClient(uri: String)(implicit csIO: ContextShift[IO]) extends GraphQLStreamingClient {

    type Subscription[F[_], D] = WebSocketSubscription[F, D]

    case class WebSocketSubscription[F[_] : LiftIO, D](stream: Stream[F, D], private val id: String) extends Stoppable[F] {
        def stop: F[Unit] = {
            LiftIO[F].liftIO(client.get.map{sender => 
                subscriptions.get(id).foreach(_.terminate())
                subscriptions -= id
                sender.foreach(_.send(Stop(id)))
            })
        }
    }

    private trait Emitter {
        def emitData(json: Json): Unit
        def terminate(): Unit
    }

    type DataQueue[F[_], D] = Queue[F, Either[Throwable, Option[D]]]

    private case class QueueEmitter[F[_] : Effect, D : Decoder](queue: DataQueue[F, D]) extends Emitter {
        def emitData(json: Json): Unit = {
            val data = json.as[D]
            val effect = queue.enqueue1(data.map(_.some))
            Effect[F].toIO(effect).unsafeRunAsyncAndForget()
        }

        def terminate(): Unit = {
            val effect = queue.enqueue1(Right(None))
            Effect[F].toIO(effect).unsafeRunAsyncAndForget()
        }
    }

    private val subscriptions: mutable.Map[String, Emitter] = mutable.Map.empty

    private val Protocol = "graphql-ws"

    private case class WebSocketSender(private val ws: WebSocket) {
        def send(msg: StreamingMessage): Unit =
            ws.send(msg.asJson.toString)
    }

    lazy private val client: Deferred[IO, Either[Exception, WebSocketSender]] = {
        val deferred = Deferred.unsafe[IO, Either[Exception, WebSocketSender]]

        try {
            val ws = new WebSocket(uri, Protocol)

            ws.onopen = { _: Event =>
                val sender = WebSocketSender(ws)
                deferred.complete(Right(sender)).map( _ =>
                    sender.send(ConnectionInit())
                ).unsafeRunAsyncAndForget()
            }

            ws.onmessage = { e: MessageEvent =>
                e.data match { 
                    case str: String => 
                        val msg = decode[StreamingMessage](str)
                        // println(msg)
                        msg match {
                            case Left(e) =>
                                println(s"Exception decoding WebSocket message for [$uri]") // TODO Proper logger
                                e.printStackTrace()
                            case Right(DataJson(id, json)) =>
                                subscriptions.get(id).foreach(_.emitData(json))
                            case _ =>
                        }
                    case other => println(s"Unexpected event from WebSocket [$uri]: [$other]")
                }
            }
            
            ws.onerror = { e: Event =>
                deferred.complete(
                    parse(js.JSON.stringify(e)).flatMap( json =>
                        Left(new GraphQLException(List(json)))
                    )
                ).recover {
                    case _: IllegalStateException => // Deferred was already complete
                        // We must cancel all subscriptions
                        subscriptions.foreach{ case(id, emitter) =>
                            emitter.terminate()
                            subscriptions -= id
                        }
                }
                .unsafeRunAsyncAndForget()
            }

            // ws.onclose // TODO Reconnect? We would have to change Deferred mechanism. Or use wrapper? Reestablish subscriptions?
        } catch {
            case e: Exception => 
                deferred.complete(Left(e)).unsafeRunAsyncAndForget()
        }

        deferred
    }

    private def buildQueue[F[_] : ConcurrentEffect, D : Decoder]: F[(String, DataQueue[F, D])] = {
        for {
            queue <- Queue.unbounded[F, Either[Throwable, Option[D]]]
        } yield {
            val id = UUID.randomUUID().toString
            val emitter = QueueEmitter(queue)
            subscriptions += (id -> emitter)
            (id, queue)
        }
    }

    def subscribe[F[_] : ConcurrentEffect, D : Decoder](subscription: String): F[Subscription[F, D]] = {
        (for {
            sender <- EitherT(LiftIO[F].liftIO(client.get))
            idq <- EitherT.right[Exception](buildQueue[F, D])
        } yield {
            val (id, q) = idq
            sender.send(Start(id, GraphQLRequest(query = subscription)))
            WebSocketSubscription(q.dequeue.rethrow.unNoneTerminate, id)
        }).value.rethrow
    }
}