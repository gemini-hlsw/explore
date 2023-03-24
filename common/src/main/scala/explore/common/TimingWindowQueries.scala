// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import cats.ApplicativeThrow
import cats.syntax.all.*
import clue.FetchClient
import clue.data.syntax.*
import explore.DefaultErrorPolicy
import explore.model.TimingWindow
import explore.model.TimingWindowEntry
import monocle.Focus
import monocle.Iso
import monocle.Lens
import queries.common.TimingWindowsGQL.*
import queries.common.TimingWindowsGQL.given
import queries.schemas.UserPreferencesDB
import queries.schemas.UserPreferencesDB.Types.*

object TimingWindowQueries:

  type TimingWindowResult = TimingWindowsQuery.Data
  val TimingWindowResult = TimingWindowsQuery.Data

  val EntryToTimingWindows: Iso[List[TimingWindowResult.TmpTimingWindows], List[TimingWindow]] =
    Iso[List[TimingWindowResult.TmpTimingWindows], List[TimingWindow]](
      _.map(TimingWindowEntry.toTimingWindow)
    )(_.map(TimingWindowEntry.fromTimingWindow))

  val TimingWindowsList: Lens[TimingWindowResult, List[TimingWindow]] =
    Focus[TimingWindowResult](_.tmpTimingWindows).andThen(EntryToTimingWindows)

  def updateTimingWindow[F[_]: ApplicativeThrow](
    tw: TimingWindow
  )(using FetchClient[F, ?, UserPreferencesDB]): F[Unit] =
    val twe = TimingWindowEntry.fromTimingWindow(tw)

    UpdateTimingWindow[F]
      .execute(
        twe.id.assign,
        TmpTimingWindowsSetInput(
          include = twe.include.assign,
          startsOn = twe.startsOn.assign,
          forever = tw.forever.assign,
          closeOn = twe.closeOn.orUnassign,
          remainOpenFor = twe.remainOpenFor.orUnassign,
          repeatPeriod = twe.repeatPeriod.orUnassign,
          repeatForever = twe.repeatForever.orUnassign,
          repeatTimes = twe.repeatTimes.orUnassign
        ).assign
      )
      .attempt
      .void
