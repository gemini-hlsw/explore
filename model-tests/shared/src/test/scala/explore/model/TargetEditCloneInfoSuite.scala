// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
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

  import TargetEditCloneInfo.BadType.*

  test("at target level, no observations") {
    val obsInfo   = TargetEditObsInfo(none, none, none, none)
    val cloneInfo = TargetEditCloneInfo.fromObsInfo(obsInfo, false)
    assertNoMessage(cloneInfo)
  }

  test("at target level, none are executed") {
    val obsInfo   = TargetEditObsInfo(none, oneTwoThree.some, none, none)
    val cloneInfo = TargetEditCloneInfo.fromObsInfo(obsInfo, false)
    assertNoMessage(cloneInfo)
  }
  test("at target level, none are executed 2") {
    val obsInfo   = TargetEditObsInfo(none, two.some, none, none)
    val cloneInfo = TargetEditCloneInfo.fromObsInfo(obsInfo, false)
    assertNoMessage(cloneInfo)
  }

  test("at target level, some are executed") {
    val obsInfo   = TargetEditObsInfo(none, oneTwoThree.some, three.some, none)
    val cloneInfo = TargetEditCloneInfo.fromObsInfo(obsInfo, false)
    assertSimple(cloneInfo, TargetEditCloneInfo.onlyNonBadMsg(Executed), oneTwo.some)
  }

  test("at target level, some are executed 2") {
    val obsInfo   = TargetEditObsInfo(none, oneTwoThree.some, oneThree.some, three.some)
    val cloneInfo = TargetEditCloneInfo.fromObsInfo(obsInfo, false)
    assertSimple(cloneInfo, TargetEditCloneInfo.onlyNonBadMsg(Executed), two.some)
  }

  test("at target level, some are executed and allowEditingOngoing not respected") {
    val obsInfo   = TargetEditObsInfo(none, oneTwoThree.some, oneThree.some, none)
    val cloneInfo = TargetEditCloneInfo.fromObsInfo(obsInfo, true)
    assertSimple(cloneInfo, TargetEditCloneInfo.onlyNonBadMsg(Executed), two.some)
  }

  test("at target level, all are executed") {
    val obsInfo   = TargetEditObsInfo(none, oneThree.some, oneThree.some, oneThree.some)
    val cloneInfo = TargetEditCloneInfo.fromObsInfo(obsInfo, false)
    assertReadonly(cloneInfo, TargetEditCloneInfo.allForTargetBadMsg(Executed))
  }

  test("at target level, all are executed 2") {
    val obsInfo   = TargetEditObsInfo(none, three.some, three.some, none)
    val cloneInfo = TargetEditCloneInfo.fromObsInfo(obsInfo, false)
    assertReadonly(cloneInfo, TargetEditCloneInfo.allForTargetBadMsg(Executed))
  }

  test("at target level, all are executed and allowEditingOngoing not respected") {
    val obsInfo   = TargetEditObsInfo(none, three.some, three.some, none)
    val cloneInfo = TargetEditCloneInfo.fromObsInfo(obsInfo, true)
    assertReadonly(cloneInfo, TargetEditCloneInfo.allForTargetBadMsg(Executed))
  }

  test("none are executed, all are being edited") {
    val obsInfo   = TargetEditObsInfo(one.some, one.some, none, none)
    val cloneInfo = TargetEditCloneInfo.fromObsInfo(obsInfo, false)
    assertNoMessage(cloneInfo)
  }

  test("none are executed, all are being edited 2") {
    val obsInfo   = TargetEditObsInfo(oneThree.some, oneThree.some, none, none)
    val cloneInfo = TargetEditCloneInfo.fromObsInfo(obsInfo, false)
    assertNoMessage(cloneInfo)
  }

  test("none are executed, all are being edited - works same with allowEditingOngoing") {
    val obsInfo   = TargetEditObsInfo(oneThree.some, oneThree.some, none, none)
    val cloneInfo = TargetEditCloneInfo.fromObsInfo(obsInfo, true)
    assertNoMessage(cloneInfo)
  }

  test("none are executed, not all are being edited") {
    val obsInfo   = TargetEditObsInfo(one.some, oneTwo.some, none, none)
    val cloneInfo = TargetEditCloneInfo.fromObsInfo(obsInfo, false)
    assertChoice(
      cloneInfo,
      TargetEditCloneInfo.otherMessage(1L, false, Executed),
      one,
      TargetEditCloneInfo.onlyThisMsg,
      none,
      TargetEditCloneInfo.allForTargetMsg(false, Executed)
    )
  }

  test("none are executed, not all are being edited 2") {
    val obsInfo   = TargetEditObsInfo(oneTwoThree.some, allFour.some, none, none)
    val cloneInfo = TargetEditCloneInfo.fromObsInfo(obsInfo, false)
    assertChoice(
      cloneInfo,
      TargetEditCloneInfo.otherMessage(1L, false, Executed),
      oneTwoThree,
      TargetEditCloneInfo.onlyCurrentMsg,
      none,
      TargetEditCloneInfo.allForTargetMsg(false, Executed)
    )
  }

  test("some are executed, all are being edited") {
    val obsInfo   = TargetEditObsInfo(allFour.some, allFour.some, oneThree.some, one.some)
    val cloneInfo = TargetEditCloneInfo.fromObsInfo(obsInfo, false)
    assertSimple(cloneInfo, TargetEditCloneInfo.onlyNonBadMsg(Executed), twoFour.some)
  }

  test("some are executed, all are being edited 2") {
    val obsInfo   = TargetEditObsInfo(oneTwo.some, oneTwo.some, one.some, one.some)
    val cloneInfo = TargetEditCloneInfo.fromObsInfo(obsInfo, false)
    assertSimple(cloneInfo, TargetEditCloneInfo.onlyNonBadMsg(Executed), two.some)
  }

  test("some of other executed") {
    val obsInfo   = TargetEditObsInfo(one.some, oneTwoThree.some, three.some, none)
    val cloneInfo = TargetEditCloneInfo.fromObsInfo(obsInfo, false)
    assertChoice(
      cloneInfo,
      TargetEditCloneInfo.otherMessage(1L, true, Executed),
      one,
      TargetEditCloneInfo.onlyThisMsg,
      oneTwo.some,
      TargetEditCloneInfo.allForTargetMsg(true, Executed)
    )
  }

  test("some of other executed 2") {
    val obsInfo   = TargetEditObsInfo(one.some, allFour.some, four.some, none)
    val cloneInfo = TargetEditCloneInfo.fromObsInfo(obsInfo, false)
    assertChoice(
      cloneInfo,
      TargetEditCloneInfo.otherMessage(2L, true, Executed),
      one,
      TargetEditCloneInfo.onlyThisMsg,
      oneTwoThree.some,
      TargetEditCloneInfo.allForTargetMsg(true, Executed)
    )
  }

  test("some of other executed 3") {
    val obsInfo   = TargetEditObsInfo(oneTwo.some, allFour.some, four.some, four.some)
    val cloneInfo = TargetEditCloneInfo.fromObsInfo(obsInfo, false)
    assertChoice(
      cloneInfo,
      TargetEditCloneInfo.otherMessage(1L, true, Executed),
      oneTwo,
      TargetEditCloneInfo.onlyCurrentMsg,
      oneTwoThree.some,
      TargetEditCloneInfo.allForTargetMsg(true, Executed)
    )
  }

  test("some of current and other executed") {
    val obsInfo   = TargetEditObsInfo(oneThree.some, allFour.some, three.some, none)
    val cloneInfo = TargetEditCloneInfo.fromObsInfo(obsInfo, false)
    assertChoice(
      cloneInfo,
      TargetEditCloneInfo.someBadMsg(Executed),
      one,
      TargetEditCloneInfo.allNonBadOfCurrentMsg(Executed),
      oneTwoFour.some,
      TargetEditCloneInfo.allNonBadMsg(Executed)
    )
  }

  test("some of current and other executed 2") {
    val obsInfo   = TargetEditObsInfo(oneTwo.some, allFour.some, oneThree.some, oneThree.some)
    val cloneInfo = TargetEditCloneInfo.fromObsInfo(obsInfo, false)
    assertChoice(
      cloneInfo,
      TargetEditCloneInfo.someBadMsg(Executed),
      two,
      TargetEditCloneInfo.allNonBadOfCurrentMsg(Executed),
      twoFour.some,
      TargetEditCloneInfo.allNonBadMsg(Executed)
    )
  }

  test("all besides current are executed") {
    val obsInfo   = TargetEditObsInfo(oneThree.some, allFour.some, twoFour.some, twoFour.some)
    val cloneInfo = TargetEditCloneInfo.fromObsInfo(obsInfo, false)
    assertSimple(cloneInfo, TargetEditCloneInfo.onlyTheseGoodMsg(Executed), oneThree.some)
  }

  test("all besides current are executed 2") {
    val obsInfo   = TargetEditObsInfo(three.some, allFour.some, oneTwoFour.some, two.some)
    val cloneInfo = TargetEditCloneInfo.fromObsInfo(obsInfo, false)
    assertSimple(cloneInfo, TargetEditCloneInfo.onlyThisGoodMsg(Executed), three.some)
  }

  test("all of current are executed") {
    val obsInfo   = TargetEditObsInfo(two.some, oneTwo.some, two.some, none)
    val cloneInfo = TargetEditCloneInfo.fromObsInfo(obsInfo, false)
    assertReadonly(cloneInfo, TargetEditCloneInfo.thisBadMsg(Executed))
  }

  test("all of current are executed 2") {
    val obsInfo   = TargetEditObsInfo(twoThree.some, allFour.some, oneTwoThree.some, none)
    val cloneInfo = TargetEditCloneInfo.fromObsInfo(obsInfo, false)
    assertReadonly(cloneInfo, TargetEditCloneInfo.allCurrentBadMsg(Executed))
  }

  test("all of current are executed 3") {
    val obsInfo   = TargetEditObsInfo(twoThree.some, allFour.some, allFour.some, none)
    val cloneInfo = TargetEditCloneInfo.fromObsInfo(obsInfo, false)
    assertReadonly(cloneInfo, TargetEditCloneInfo.allCurrentBadMsg(Executed))
  }

  test("allowEditingOngoing - some are ongoing, none are complete, all are being edited") {
    val obsInfo   = TargetEditObsInfo(one.some, one.some, one.some, none)
    val cloneInfo = TargetEditCloneInfo.fromObsInfo(obsInfo, true)
    assertNoMessage(cloneInfo)
  }

  test("allowEditingOngoing - some are ongoing, none are complete, all are being edited 2") {
    val obsInfo   = TargetEditObsInfo(oneThree.some, oneThree.some, oneThree.some, none)
    val cloneInfo = TargetEditCloneInfo.fromObsInfo(obsInfo, true)
    assertNoMessage(cloneInfo)
  }

  test("allowEditingOngoing - some of other completed") {
    val obsInfo   = TargetEditObsInfo(one.some, oneTwoThree.some, twoThree.some, three.some)
    val cloneInfo = TargetEditCloneInfo.fromObsInfo(obsInfo, true)
    assertChoice(
      cloneInfo,
      TargetEditCloneInfo.otherMessage(1L, true, Completed),
      one,
      TargetEditCloneInfo.onlyThisMsg,
      oneTwo.some,
      TargetEditCloneInfo.allForTargetMsg(true, Completed)
    )
  }

  test("allowEditingOngoing - some of other completed 2") {
    val obsInfo   = TargetEditObsInfo(oneTwo.some, allFour.some, twoFour.some, four.some)
    val cloneInfo = TargetEditCloneInfo.fromObsInfo(obsInfo, true)
    assertChoice(
      cloneInfo,
      TargetEditCloneInfo.otherMessage(1L, true, Completed),
      oneTwo,
      TargetEditCloneInfo.onlyCurrentMsg,
      oneTwoThree.some,
      TargetEditCloneInfo.allForTargetMsg(true, Completed)
    )
  }

  test("allowEditingOngoing - some of current and other completed") {
    val obsInfo   = TargetEditObsInfo(oneTwo.some, allFour.some, allFour.some, twoFour.some)
    val cloneInfo = TargetEditCloneInfo.fromObsInfo(obsInfo, true)
    assertChoice(
      cloneInfo,
      TargetEditCloneInfo.someBadMsg(Completed),
      one,
      TargetEditCloneInfo.allNonBadOfCurrentMsg(Completed),
      oneThree.some,
      TargetEditCloneInfo.allNonBadMsg(Completed)
    )
  }

  test("allowEditingOngoing - all besides current are completed") {
    val obsInfo   = TargetEditObsInfo(oneThree.some, oneTwoThree.some, twoThree.some, two.some)
    val cloneInfo = TargetEditCloneInfo.fromObsInfo(obsInfo, true)
    assertSimple(cloneInfo, TargetEditCloneInfo.onlyTheseGoodMsg(Completed), oneThree.some)
  }

  test("allowEditingOngoing - all besides current are completed 2") {
    val obsInfo   = TargetEditObsInfo(one.some, oneTwoThree.some, twoThree.some, twoThree.some)
    val cloneInfo = TargetEditCloneInfo.fromObsInfo(obsInfo, true)
    assertSimple(cloneInfo, TargetEditCloneInfo.onlyThisGoodMsg(Completed), one.some)
  }

  test("allowEditingOngoing - all of current are completed") {
    val obsInfo   = TargetEditObsInfo(one.some, oneTwoThree.some, oneTwoThree.some, one.some)
    val cloneInfo = TargetEditCloneInfo.fromObsInfo(obsInfo, true)
    assertReadonly(cloneInfo, TargetEditCloneInfo.thisBadMsg(Completed))
  }

  test("allowEditingOngoing - all of current are completed 2") {
    val obsInfo   =
      TargetEditObsInfo(oneTwo.some, oneTwoThree.some, oneTwoThree.some, oneTwoThree.some)
    val cloneInfo = TargetEditCloneInfo.fromObsInfo(obsInfo, true)
    assertReadonly(cloneInfo, TargetEditCloneInfo.allCurrentBadMsg(Completed))
  }

}
