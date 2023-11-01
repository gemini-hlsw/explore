// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.utils

import cats.effect
import cats.effect.IO
import cats.effect.testkit.TestControl
import fs2.Stream
import munit.Location

import scala.concurrent.duration.*

class StreamSpec extends munit.CatsEffectSuite:

  // Stream that emits 1 every 100ms, sleeps for 1 second, and repeats
  val stream =
    (Stream.awakeEvery[IO](50.millis).as(1).take(4) ++ Stream.sleep_[IO](100.millis)).repeat

  val sut = stream.reduceWithin(250.millis, _ + _)

  test("reduceWithin combines elements within a time window") {
    val program = sut.head.compile.lastOrError

    TestControl.executeEmbed(program).assertEquals(4)
  }

  test("reduceWithin only combines within given time window") {
    val program = sut.take(5).compile.toVector

    TestControl.executeEmbed(program).assertEquals(Vector(4, 4, 4, 4, 4))
  }

  test("reduceWithin does not combine if elements are outside the time window") {
    val program = Stream
      .awakeEvery[IO](100.millis)
      .as(1)
      .take(5)
      .reduceWithin(50.millis, _ + _)
      .compile
      .toVector

    TestControl.executeEmbed(program).assertEquals(Vector(1, 1, 1, 1, 1))
  }

  test("reduceWithinSemigroup uses semigroup to combine") {
    val program = stream.reduceSemigroupWithin(250.millis).head.compile.lastOrError

    TestControl.executeEmbed(program).assertEquals(4)
  }
