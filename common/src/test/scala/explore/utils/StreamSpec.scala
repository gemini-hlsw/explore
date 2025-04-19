// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.utils

import cats.effect
import cats.effect.IO
import cats.effect.testkit.TestControl
import cats.syntax.all.*
import fs2.Stream
import munit.Location

import scala.concurrent.duration.*

class StreamSpec extends munit.CatsEffectSuite:

  // Stream that emits 1 every 0.5s, sleeps for 1s, and repeats
  val stream =
    (Stream.awakeEvery[IO](0.5.seconds).as(1).take(4) ++ Stream.sleep_[IO](1.second)).repeat

  val sut = stream.reduceWithin(2.5.seconds, _ + _)

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
      .awakeEvery[IO](1.second)
      .as(1)
      .take(5)
      .reduceWithin(0.5.seconds, _ + _)
      .compile
      .toVector

    TestControl.executeEmbed(program).assertEquals(Vector(1, 1, 1, 1, 1))
  }

  test("reduceSemigroupWithin uses semigroup to combine") {
    val program = stream.reduceSemigroupWithin(2.5.seconds).head.compile.lastOrError

    TestControl.executeEmbed(program).assertEquals(4)
  }

  test("different behaviour to groupWithin") {
    val a = stream.reduceSemigroupWithin(3.seconds)
    val b = stream.groupWithin(Int.MaxValue, 3.seconds).map(_.combineAll)

    val program = (a.take(5).compile.toVector, b.take(5).compile.toVector).tupled

    TestControl.executeEmbed(program).map((a, b) => assertNotEquals(a, b))
  }

  test("keyedSwitchEvalMap cancels selectively") {
    val stream                 = Stream(1, 2, 3).meteredStartImmediately[IO](0.25.seconds)
    val effect: Int => IO[Int] = i => IO.sleep(1.second).as(i)
    // The key is _ % 2, so the first element should be cancelled when the third one is emitted:
    // At 0s: 1 is emitted in the input and its 1s `effect` starts (A).
    // At 0.25s: 2 is emitted in the input and its 1s `effect` starts.
    // At 0.5s: 3 is emitted in the input and its 1s `effect` starts, cancelling the first one (A).
    // At 1.25s: 2 is emitted in the output.
    // At 1.5s: 3 is emitted in the output.
    val pipe                   = keyedSwitchEvalMap[IO, Int, Int, Int](_ % 2, effect)

    val program = stream.through(pipe).compile.toVector

    TestControl.executeEmbed(program).assertEquals(Vector(2, 3))
  }

  test("keyedSwitchEvalMap emits all") {
    val stream                 = Stream(1, 2, 3).meteredStartImmediately[IO](0.5.seconds)
    val effect: Int => IO[Int] = i => IO.sleep(0.25.second).as(i)
    // All elements should be emitted, no cancellations occur.
    // At 0s: 1 is emitted in the input and its 0.25s `effect` starts.
    // At 0.25s: 1 is emitted in the output.
    // At 0.5s: 2 is emitted in the input and its 1s `effect` starts.
    // At 0.75s: 2 is emitted in the output.
    // At 1s: 3 is emitted in the input and its 1s `effect` starts.
    // At 1.25s: 3 is emitted in the output.
    val pipe                   = keyedSwitchEvalMap[IO, Int, Int, Int](_ % 2, effect)

    val program = stream.through(pipe).compile.toVector

    TestControl.executeEmbed(program).assertEquals(Vector(1, 2, 3))
  }
