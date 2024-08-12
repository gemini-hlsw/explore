// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
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
  // if it is readonly, there should always be a message
  def readonlyMsg: Option[NonEmptyString]              =
    if (readonly) message else none
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
  def noMessages: TargetEditCloneInfo                                                        = TargetEditCloneInfo(false)

  val onlyThisMsg: NonEmptyString             = "only this observation".refined
  val onlyCurrentMsg: NonEmptyString          = "only the current observations".refined
  val allCurrentExecutedMsg: NonEmptyString   =
    "All the current observations have been executed. Target is readonly.".refined
  val thisExecutedMsg: NonEmptyString         =
    "The current observation has been executed. Target is readonly.".refined
  val allForTargetExecutedMsg: NonEmptyString =
    "All associated observations have been executed. Target is readonly".refined
  val onlyUnexecutedMsg: NonEmptyString       =
    "Target will only be modified for the un-executed observations.".refined
  val someExecutedMsg: NonEmptyString         =
    "Some of the observations being edited have been executed. Edits should apply to ".refined
  val unexecutedOfCurrentMsg: NonEmptyString  =
    "unexecuted observations of the current asterism".refined
  val allUnexectedMsg: NonEmptyString         = "all unexecuted observations".refined
  val allThisMsg: NonEmptyString              =
    "Target will only be modified for this observation. All other observations have been executed.".refined
  val allCurrentMsg: NonEmptyString           =
    "Target will only be modified for the current observations. All other observations have been executed.".refined

  extension (obsIds: ObsIdSet)
    def onlyCurrentText: NonEmptyString      =
      if (obsIds.size === 1) onlyThisMsg
      else onlyCurrentMsg
    def allOtherExecutedText: NonEmptyString =
      if (obsIds.size === 1) allThisMsg
      else allCurrentMsg
    def allExecutedMsg: NonEmptyString       =
      if (obsIds.size === 1) thisExecutedMsg
      else allCurrentExecutedMsg

  def otherMessage(otherCount: Long, hasExecuted: Boolean): NonEmptyString =
    val plural = if (otherCount === 1) "" else "s"
    val ex     = if (hasExecuted) "unexecuted " else ""
    NonEmptyString.unsafeFrom(
      s"Target is in $otherCount other ${ex}observation$plural. Edits here should apply to "
    )

  def allForTargetMsg(hasExecuted: Boolean): NonEmptyString =
    val ex = if (hasExecuted) "unexecuted " else ""
    NonEmptyString.unsafeFrom(s"all ${ex}observations of this target")

  def fromObsInfo(obsInfo: TargetEditObsInfo): TargetEditCloneInfo =
    obsInfo.current match
      // We're editing at the target level (not for an asterism)
      case None                                                   =>
        if (obsInfo.allForTargetAreExecuted)
          TargetEditCloneInfo.readonly(allForTargetExecutedMsg)
        else if (obsInfo.allForTargetAreOK) TargetEditCloneInfo.noMessages
        else
          TargetEditCloneInfo.simple(
            onlyUnexecutedMsg,
            obsInfo.unexecutedForTarget
          )
      case Some(editing) if obsInfo.allCurrentAreExecuted         =>
        TargetEditCloneInfo.readonly(editing.allExecutedMsg)
      // There are no other unexecuted observations, but maybe there are others.
      case Some(editing) if obsInfo.otherUnexecutedObsCount === 0 =>
        if (obsInfo.allCurrentAreOK && obsInfo.otherObsCount === 0)
          TargetEditCloneInfo.noMessages // We're editing all, and they're OK
        else if (obsInfo.allCurrentAreOK)
          // we're not editing all of them and they're OK, but the rest are executed.
          TargetEditCloneInfo.simple(
            editing.allOtherExecutedText,
            editing.some
          )
        else
          // We're not editing all of them, but some are executed, as are any others.
          TargetEditCloneInfo.simple(
            onlyUnexecutedMsg,
            obsInfo.unexecutedForTarget
          )
      // There are some other unexecuted observations
      case Some(editing)                                          =>
        if (obsInfo.allForTargetAreOK)
          TargetEditCloneInfo.choice(
            otherMessage(obsInfo.otherObsCount, false),
            editing.some,
            editing.onlyCurrentText,
            none,
            allForTargetMsg(false)
          )
        else if (obsInfo.allCurrentAreOK) // some of the other observations have been executed.
          TargetEditCloneInfo.choice(
            otherMessage(obsInfo.otherUnexecutedObsCount, true),
            editing.some,
            editing.onlyCurrentText,
            obsInfo.unexecutedForTarget,
            allForTargetMsg(true)
          )
        else                              // some of observations being edited have been executed, too
          TargetEditCloneInfo.choice(
            someExecutedMsg,
            obsInfo.unexecutedForCurrent,
            unexecutedOfCurrentMsg,
            obsInfo.unexecutedForTarget,
            allUnexectedMsg
          )
