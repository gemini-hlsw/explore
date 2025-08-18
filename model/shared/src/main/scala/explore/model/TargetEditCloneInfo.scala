// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.refined.*

// Class used by TargetCloneSelector to determine what observations a target needs to
// be cloned for and what messages to display.
final case class TargetEditCloneInfo(
  readonly:            Boolean,
  message:             Option[NonEmptyString] = None,
  cloneForCurrent:     Option[ObsIdSet] = None, // also used for the "simple" case
  cloneForCurrentText: Option[NonEmptyString] = None,
  cloneForAll:         Option[ObsIdSet] = None,
  cloneForAllText:     Option[NonEmptyString] = None
) derives Eq:
  def choice: Option[(NonEmptyString, NonEmptyString)] =
    (message, cloneForCurrentText, cloneForAllText) match
      case (Some(_), Some(current), Some(all)) => (current, all).some
      case _                                   => none
  def noMessages: Boolean                              =
    readonly === false && message.isEmpty

object TargetEditCloneInfo:
  // just a message, no choices
  def simple(simpleMessage: NonEmptyString, cloneFor: Option[ObsIdSet]): TargetEditCloneInfo =
    TargetEditCloneInfo(false, simpleMessage.some, cloneFor)
  def readonly(message: NonEmptyString): TargetEditCloneInfo                                 =
    TargetEditCloneInfo(true, message.some)
  def choice(
    message:         NonEmptyString,
    cloneForOnly:    Option[ObsIdSet],
    onlyCurrentMsg:  NonEmptyString,
    cloneForAll:     Option[ObsIdSet],
    cloneForAllText: NonEmptyString
  ): TargetEditCloneInfo =
    TargetEditCloneInfo(false,
                        message.some,
                        cloneForOnly,
                        onlyCurrentMsg.some,
                        cloneForAll,
                        cloneForAllText.some
    )
  def noMessages: TargetEditCloneInfo                                                        =
    TargetEditCloneInfo(false)

  enum BadType(val text: String):
    case Executed  extends BadType("executed")
    case Completed extends BadType("completed")

  import BadType.*

  val onlyThisMsg: NonEmptyString                             = "only this observation".refined
  val onlyCurrentMsg: NonEmptyString                          = "only the current observations".refined
  def allCurrentBadMsg(badType: BadType): NonEmptyString      =
    NonEmptyString.unsafeFrom(
      s"All the current observations have been ${badType.text}. Target is readonly."
    )
  def thisBadMsg(badType: BadType): NonEmptyString            =
    NonEmptyString.unsafeFrom(
      s"The current observation has been ${badType.text}. Target is readonly."
    )
  def allForTargetBadMsg(badType: BadType): NonEmptyString    =
    NonEmptyString.unsafeFrom(
      s"All associated observations have been ${badType.text}. Target is readonly"
    )
  def allNonBadOfCurrentMsg(badType: BadType): NonEmptyString =
    NonEmptyString.unsafeFrom(
      s"non-${badType.text} observations of the current asterism"
    )
  def onlyNonBadMsg(badType: BadType): NonEmptyString         =
    NonEmptyString.unsafeFrom(
      s"Target will only be modified for the non-${badType.text} observations."
    )
  def someBadMsg(badType: BadType): NonEmptyString            =
    NonEmptyString.unsafeFrom(
      s"Some of the observations being edited have been ${badType.text}. Edits should apply to "
    )
  def allNonBadMsg(badType: BadType): NonEmptyString          =
    NonEmptyString.unsafeFrom(s"all non-${badType.text} observations")
  def onlyThisGoodMsg(badType: BadType): NonEmptyString       =
    NonEmptyString.unsafeFrom(
      s"Target will only be modified for this observation. All other observations have been ${badType.text}."
    )
  def onlyTheseGoodMsg(badType: BadType): NonEmptyString      =
    NonEmptyString.unsafeFrom(
      s"Target will only be modified for the current observations. All other observations have been ${badType.text}."
    )

  extension (obsIds: ObsIdSet)
    def onlyCurrentText: NonEmptyString                   =
      if (obsIds.size === 1) onlyThisMsg
      else onlyCurrentMsg
    def allOtherBadText(badType: BadType): NonEmptyString =
      if (obsIds.size === 1) onlyThisGoodMsg(badType)
      else onlyTheseGoodMsg(badType)
    def allBadMsg(badType: BadType): NonEmptyString       =
      if (obsIds.size === 1) thisBadMsg(badType)
      else allCurrentBadMsg(badType)

  def otherMessage(otherCount: Long, hasBad: Boolean, badType: BadType): NonEmptyString =
    val plural = if (otherCount === 1) "" else "s"
    val ex     = if (hasBad) s"non-${badType.text} " else ""
    NonEmptyString.unsafeFrom(
      s"Target is in $otherCount other ${ex}observation$plural. Edits here should apply to "
    )

  def allForTargetMsg(hasBad: Boolean, badType: BadType): NonEmptyString =
    val ex = if (hasBad) s"non-${badType.text} " else ""
    NonEmptyString.unsafeFrom(s"all ${ex}observations of this target")

  def fromObsInfo(
    obsInfo:             TargetEditObsInfo,
    allowEditingOngoing: Boolean
  ): TargetEditCloneInfo =
    obsInfo.current match
      // We're editing at the target level (not for an asterism).
      // right now, we don't honor the allowEditingOngoing flag for this case.
      case None                                                                          =>
        if (obsInfo.allForTargetAreExecuted)
          TargetEditCloneInfo.readonly(allForTargetBadMsg(Executed))
        else if (obsInfo.allForTargetAreUnexecuted) TargetEditCloneInfo.noMessages
        else
          TargetEditCloneInfo.simple(
            onlyNonBadMsg(Executed),
            obsInfo.unexecutedForTarget
          )
      case Some(editing) if allowEditingOngoing && obsInfo.allCurrentAreCompleted        =>
        TargetEditCloneInfo.readonly(editing.allBadMsg(Completed))
      // There no other incomplete observations, but there may be other observations.
      case Some(editing) if allowEditingOngoing && obsInfo.otherIncompleteObsCount === 0 =>
        if (obsInfo.allCurrentAreIncomplete && obsInfo.otherObsCount === 0)
          // TODO: Put a message here about the observation being executed????
          TargetEditCloneInfo.noMessages // We're editing all, and they're OK
        else if (obsInfo.allCurrentAreIncomplete)
          // we're not editing all of them and they're OK, but the rest are completed.
          TargetEditCloneInfo.simple(
            editing.allOtherBadText(Completed),
            editing.some
          )
        else
          // We're not editing all of them, but some are executed, as are any others.
          TargetEditCloneInfo.simple(
            onlyNonBadMsg(Completed),
            obsInfo.incompleteForTarget
          )
      // There are some other non-completed observations
      case (Some(editing)) if allowEditingOngoing                                        =>
        if (obsInfo.allForTargetAreIncomplete)
          TargetEditCloneInfo.choice(
            otherMessage(obsInfo.otherObsCount, false, Completed),
            editing.some,
            editing.onlyCurrentText,
            none,
            allForTargetMsg(false, Completed)
          )
        else if (obsInfo.allCurrentAreIncomplete)
          // some of the other observations have been completed.
          TargetEditCloneInfo.choice(
            otherMessage(obsInfo.otherIncompleteObsCount, true, Completed),
            editing.some,
            editing.onlyCurrentText,
            obsInfo.incompleteForTarget,
            allForTargetMsg(true, Completed)
          )
        else // some of the observations being edited have been completed, too
          TargetEditCloneInfo.choice(
            someBadMsg(Completed),
            obsInfo.incompleteForCurrent,
            allNonBadOfCurrentMsg(Completed),
            obsInfo.incompleteForTarget,
            allNonBadMsg(Completed)
          )
      case Some(editing) if obsInfo.allCurrentAreExecuted                                =>
        TargetEditCloneInfo.readonly(editing.allBadMsg(Executed))
      // There are no other unexecuted observations, but there may be other observations.
      case Some(editing) if obsInfo.otherUnexecutedObsCount === 0                        =>
        if (obsInfo.allCurrentAreUnexecuted && obsInfo.otherObsCount === 0)
          TargetEditCloneInfo.noMessages // We're editing all, and they're OK
        else if (obsInfo.allCurrentAreUnexecuted)
          // we're not editing all of them and they're OK, but the rest are executed.
          TargetEditCloneInfo.simple(
            editing.allOtherBadText(Executed),
            editing.some
          )
        else
          // We're not editing all of them, but some are executed, as are any others.
          TargetEditCloneInfo.simple(
            onlyNonBadMsg(Executed),
            obsInfo.unexecutedForTarget
          )
      // There are some other unexecuted observations
      case Some(editing)                                                                 =>
        if (obsInfo.allForTargetAreUnexecuted)
          TargetEditCloneInfo.choice(
            otherMessage(obsInfo.otherObsCount, false, Executed),
            editing.some,
            editing.onlyCurrentText,
            none,
            allForTargetMsg(false, Executed)
          )
        else if (obsInfo.allCurrentAreUnexecuted)
          // some of the other observations have been executed.
          TargetEditCloneInfo.choice(
            otherMessage(obsInfo.otherUnexecutedObsCount, true, Executed),
            editing.some,
            editing.onlyCurrentText,
            obsInfo.unexecutedForTarget,
            allForTargetMsg(true, Executed)
          )
        else // some of the observations being edited have been executed, too
          TargetEditCloneInfo.choice(
            someBadMsg(Executed),
            obsInfo.unexecutedForCurrent,
            allNonBadOfCurrentMsg(Executed),
            obsInfo.unexecutedForTarget,
            allNonBadMsg(Executed)
          )
