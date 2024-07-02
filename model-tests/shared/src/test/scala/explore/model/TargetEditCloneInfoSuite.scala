// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.model.Observation
import munit.FunSuite
import munit.Location

trait MyAssertions extends munit.Assertions {
  private def fullMsg(ci: TargetEditCloneInfo, fieldMsg: String) =
    s"$fieldMsg\nTargetEditCloneInfo was $ci"

  private def assertStrEq(
    ci:       TargetEditCloneInfo,
    current:  Option[NonEmptyString],
    expected: Option[NonEmptyString],
    field:    String
  )(using loc: Location): Unit =
    assertEquals(current, expected, fullMsg(ci, s"$field should be '$expected'"))

  private def assertMessage(ci: TargetEditCloneInfo, expected: Option[NonEmptyString])(using
    loc: Location
  ): Unit =
    assertStrEq(ci, ci.message, expected, "message")

  private def assertCurrent(ci: TargetEditCloneInfo, expected: Option[ObsIdSet])(using
    loc: Location
  ) =
    assertEquals(ci.cloneForCurrent,
                 expected,
                 fullMsg(ci, s"cloneForCurrent should be '$expected'")
    )

  private def assertForAll(ci: TargetEditCloneInfo, expected: Option[ObsIdSet])(using
    loc: Location
  ) =
    assertEquals(ci.cloneForAll, expected, fullMsg(ci, s"cloneForAll should be '$expected'"))

  private def assertCurrentText(ci: TargetEditCloneInfo, expected: Option[NonEmptyString])(using
    loc: Location
  ): Unit =
    assertStrEq(ci, ci.cloneForCurrentText, expected, "cloneForCurrentText")

  private def assertForAllText(ci: TargetEditCloneInfo, expected: Option[NonEmptyString])(using
    loc: Location
  ): Unit =
    assertStrEq(ci, ci.cloneForAllText, expected, "cloneForAllText")

  private def assertIsReadonly(ci: TargetEditCloneInfo)(using loc: Location) =
    assert(ci.readonly, fullMsg(ci, "Should be readonly"))

  private def assertIsNotReadonly(ci: TargetEditCloneInfo)(using loc: Location) =
    assert(!ci.readonly, fullMsg(ci, "Should not be readonly"))

  def assertChoice(
    ci:          TargetEditCloneInfo,
    message:     NonEmptyString,
    current:     ObsIdSet,
    currentText: NonEmptyString,
    all:         Option[ObsIdSet],
    allText:     NonEmptyString
  )(using loc: Location): Unit =
    assertIsNotReadonly(ci)
    assertMessage(ci, message.some)
    assertCurrent(ci, current.some)
    assertCurrentText(ci, currentText.some)
    assertForAll(ci, all)
    assertForAllText(ci, allText.some)

  def assertSimple(
    ci:         TargetEditCloneInfo,
    message:    NonEmptyString,
    toCloneFor: Option[ObsIdSet]
  )(using loc: Location): Unit =
    assertIsNotReadonly(ci)
    assertMessage(ci, message.some)
    assertCurrent(ci, toCloneFor)
    assertCurrentText(ci, none)
    assertForAll(ci, none)
    assertForAllText(ci, none)

  def assertNoMessage(ci: TargetEditCloneInfo)(using loc: Location): Unit =
    assertIsNotReadonly(ci)
    assertMessage(ci, none)
    assertCurrent(ci, none)
    assertCurrentText(ci, none)
    assertForAll(ci, none)
    assertForAllText(ci, none)

  def assertReadonly(ci: TargetEditCloneInfo, message: NonEmptyString)(using loc: Location): Unit =
    assertIsReadonly(ci)
    assertMessage(ci, message.some)
    assertCurrent(ci, none)
    assertCurrentText(ci, none)
    assertForAll(ci, none)
    assertForAllText(ci, none)

}

class TargetEditCloneInfoSuite extends FunSuite with MyAssertions {
  val o1          = Observation.Id.fromLong(1L).get
  val o2          = Observation.Id.fromLong(2L).get
  val o3          = Observation.Id.fromLong(3L).get
  val o4          = Observation.Id.fromLong(4L).get
  val one         = ObsIdSet.one(o1)
  val two         = ObsIdSet.one(o2)
  val three       = ObsIdSet.one(o3)
  val four        = ObsIdSet.one(o4)
  val oneTwo      = ObsIdSet.of(o1, o2)
  val oneThree    = ObsIdSet.of(o1, o3)
  val oneTwoThree = ObsIdSet.of(o1, o2, o3)
  val twoThree    = ObsIdSet.of(o2, o3)
  val twoFour     = ObsIdSet.of(o2, o4)
  val oneTwoFour  = ObsIdSet.of(o1, o2, o4)
  val threeFour   = ObsIdSet.of(o3, o4)
  val allFour     = ObsIdSet.of(o1, o2, o3, o4)

  test("at target level, no observations") {
    val obsInfo   = TargetEditObsInfo(none, none, none)
    val cloneInfo = TargetEditCloneInfo.fromObsInfo(obsInfo)
    assertNoMessage(cloneInfo)
  }

  test("at target level, none are executed") {
    val obsInfo   = TargetEditObsInfo(none, oneTwoThree.some, none)
    val cloneInfo = TargetEditCloneInfo.fromObsInfo(obsInfo)
    assertNoMessage(cloneInfo)
  }
  test("at target level, none are executed 2") {
    val obsInfo   = TargetEditObsInfo(none, two.some, none)
    val cloneInfo = TargetEditCloneInfo.fromObsInfo(obsInfo)
    assertNoMessage(cloneInfo)
  }

  test("at target level, some are executed") {
    val obsInfo   = TargetEditObsInfo(none, oneTwoThree.some, three.some)
    val cloneInfo = TargetEditCloneInfo.fromObsInfo(obsInfo)
    assertSimple(cloneInfo, TargetEditCloneInfo.onlyUnexecutedMsg, oneTwo.some)
  }

  test("at target level, some are executed 2") {
    val obsInfo   = TargetEditObsInfo(none, oneTwoThree.some, oneThree.some)
    val cloneInfo = TargetEditCloneInfo.fromObsInfo(obsInfo)
    assertSimple(cloneInfo, TargetEditCloneInfo.onlyUnexecutedMsg, two.some)
  }

  test("at target level, all are executed") {
    val obsInfo   = TargetEditObsInfo(none, oneThree.some, oneThree.some)
    val cloneInfo = TargetEditCloneInfo.fromObsInfo(obsInfo)
    assertReadonly(cloneInfo, TargetEditCloneInfo.allForTargetExecutedMsg)
  }

  test("at target level, all are executed 2") {
    val obsInfo   = TargetEditObsInfo(none, three.some, three.some)
    val cloneInfo = TargetEditCloneInfo.fromObsInfo(obsInfo)
    assertReadonly(cloneInfo, TargetEditCloneInfo.allForTargetExecutedMsg)
  }

  test("none are executed, all are being edited") {
    val obsInfo   = TargetEditObsInfo(one.some, one.some, none)
    val cloneInfo = TargetEditCloneInfo.fromObsInfo(obsInfo)
    assertNoMessage(cloneInfo)
  }

  test("none are executed, all are being edited 2") {
    val obsInfo   = TargetEditObsInfo(oneThree.some, oneThree.some, none)
    val cloneInfo = TargetEditCloneInfo.fromObsInfo(obsInfo)
    assertNoMessage(cloneInfo)
  }

  test("none are executed, not all are being edited") {
    val obsInfo   = TargetEditObsInfo(one.some, oneTwo.some, none)
    val cloneInfo = TargetEditCloneInfo.fromObsInfo(obsInfo)
    assertChoice(
      cloneInfo,
      TargetEditCloneInfo.otherMessage(1L, false),
      one,
      TargetEditCloneInfo.onlyThisMsg,
      none,
      TargetEditCloneInfo.allForTargetMsg(false)
    )
  }

  test("none are executed, not all are being edited 2") {
    val obsInfo   = TargetEditObsInfo(oneTwoThree.some, allFour.some, none)
    val cloneInfo = TargetEditCloneInfo.fromObsInfo(obsInfo)
    assertChoice(
      cloneInfo,
      TargetEditCloneInfo.otherMessage(1L, false),
      oneTwoThree,
      TargetEditCloneInfo.onlyCurrentMsg,
      none,
      TargetEditCloneInfo.allForTargetMsg(false)
    )
  }

  test("some are executed, all are being edited") {
    val obsInfo   = TargetEditObsInfo(allFour.some, allFour.some, oneThree.some)
    val cloneInfo = TargetEditCloneInfo.fromObsInfo(obsInfo)
    assertSimple(cloneInfo, TargetEditCloneInfo.onlyUnexecutedMsg, twoFour.some)
  }

  test("some are executed, all are being edited 2") {
    val obsInfo   = TargetEditObsInfo(oneTwo.some, oneTwo.some, one.some)
    val cloneInfo = TargetEditCloneInfo.fromObsInfo(obsInfo)
    assertSimple(cloneInfo, TargetEditCloneInfo.onlyUnexecutedMsg, two.some)
  }

  test("some of other executed") {
    val obsInfo   = TargetEditObsInfo(one.some, oneTwoThree.some, three.some)
    val cloneInfo = TargetEditCloneInfo.fromObsInfo(obsInfo)
    assertChoice(
      cloneInfo,
      TargetEditCloneInfo.otherMessage(1L, true),
      one,
      TargetEditCloneInfo.onlyThisMsg,
      oneTwo.some,
      TargetEditCloneInfo.allForTargetMsg(true)
    )
  }

  test("some of other executed 2") {
    val obsInfo   = TargetEditObsInfo(one.some, allFour.some, four.some)
    val cloneInfo = TargetEditCloneInfo.fromObsInfo(obsInfo)
    assertChoice(
      cloneInfo,
      TargetEditCloneInfo.otherMessage(2L, true),
      one,
      TargetEditCloneInfo.onlyThisMsg,
      oneTwoThree.some,
      TargetEditCloneInfo.allForTargetMsg(true)
    )
  }

  test("some of other executed 3") {
    val obsInfo   = TargetEditObsInfo(oneTwo.some, allFour.some, four.some)
    val cloneInfo = TargetEditCloneInfo.fromObsInfo(obsInfo)
    assertChoice(
      cloneInfo,
      TargetEditCloneInfo.otherMessage(1L, true),
      oneTwo,
      TargetEditCloneInfo.onlyCurrentMsg,
      oneTwoThree.some,
      TargetEditCloneInfo.allForTargetMsg(true)
    )
  }

  test("some of current and other executed") {
    val obsInfo   = TargetEditObsInfo(oneThree.some, allFour.some, three.some)
    val cloneInfo = TargetEditCloneInfo.fromObsInfo(obsInfo)
    assertChoice(
      cloneInfo,
      TargetEditCloneInfo.someExecutedMsg,
      one,
      TargetEditCloneInfo.unexecutedOfCurrentMsg,
      oneTwoFour.some,
      TargetEditCloneInfo.allUnexectedMsg
    )
  }

  test("some of current and other executed 2") {
    val obsInfo   = TargetEditObsInfo(oneTwo.some, allFour.some, oneThree.some)
    val cloneInfo = TargetEditCloneInfo.fromObsInfo(obsInfo)
    assertChoice(
      cloneInfo,
      TargetEditCloneInfo.someExecutedMsg,
      two,
      TargetEditCloneInfo.unexecutedOfCurrentMsg,
      twoFour.some,
      TargetEditCloneInfo.allUnexectedMsg
    )
  }

  test("all besides current are executed") {
    val obsInfo   = TargetEditObsInfo(oneThree.some, allFour.some, twoFour.some)
    val cloneInfo = TargetEditCloneInfo.fromObsInfo(obsInfo)
    assertNoMessage(cloneInfo)
  }

  test("all besides current are executed 2") {
    val obsInfo   = TargetEditObsInfo(three.some, allFour.some, oneTwoFour.some)
    val cloneInfo = TargetEditCloneInfo.fromObsInfo(obsInfo)
    assertNoMessage(cloneInfo)
  }

  test("all of current are executed") {
    val obsInfo   = TargetEditObsInfo(two.some, oneTwo.some, two.some)
    val cloneInfo = TargetEditCloneInfo.fromObsInfo(obsInfo)
    assertReadonly(cloneInfo, TargetEditCloneInfo.allCurrentExecutedMsg)
  }

  test("all of current are executed 2") {
    val obsInfo   = TargetEditObsInfo(twoThree.some, allFour.some, oneTwoThree.some)
    val cloneInfo = TargetEditCloneInfo.fromObsInfo(obsInfo)
    assertReadonly(cloneInfo, TargetEditCloneInfo.allCurrentExecutedMsg)
  }

  test("all of current are executed 3") {
    val obsInfo   = TargetEditObsInfo(twoThree.some, allFour.some, allFour.some)
    val cloneInfo = TargetEditCloneInfo.fromObsInfo(obsInfo)
    assertReadonly(cloneInfo, TargetEditCloneInfo.allCurrentExecutedMsg)
  }

}
