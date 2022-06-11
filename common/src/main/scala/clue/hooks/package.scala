// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package clue

import japgolly.scalajs.react._
import io.circe.Decoder
import clue.StreamingClient
import japgolly.scalajs.react.util.DefaultEffects.{Async => DefaultA}
import clue.GraphQLOperation
import io.circe.Encoder
import io.circe.Json
import cats.effect.Resource

package object hooks extends UseSubscription.HooksApiExt {
  implicit class StringGQLOps(val str: String) extends AnyVal {
    def subscription[S, A](implicit
      client:  StreamingClient[DefaultA, S],
      decoder: Decoder[A]
    ): GraphQLSubscription[S, A] = GraphQLSubscription(str)
  }
}

package hooks {
  final case class GraphQLSubscription[S, A](document: String)(implicit
    client:                                            StreamingClient[DefaultA, S],
    decoder:                                           Decoder[A]
  ) {
    def subscribe: Resource[DefaultA, fs2.Stream[DefaultA, A]] =
      client.subscribe(new GraphQLOperation[S] {
        val document = GraphQLSubscription.this.document

        type Variables = Unit
        type Data      = A

        val varEncoder  = Encoder.instance(_ => Json.Null)
        val dataDecoder = decoder
      })
  }
}
