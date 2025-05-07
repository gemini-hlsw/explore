// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.effect.IO
import cats.syntax.all.*
import clue.FetchClient
import crystal.react.*
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.model.ObsIdSet
import explore.model.ObservationsAndTargets
import explore.model.OnAsterismUpdateParams
import explore.services.OdbTargetApi
import explore.syntax.ui.*
import explore.targets.TargetSelectionPopup
import explore.targets.TargetSource
import explore.undo.UndoSetter
import japgolly.scalajs.react.*
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.react.common.Css
import lucuma.react.primereact.Button
import lucuma.schemas.ObservationDB
import lucuma.schemas.model.TargetWithId
import lucuma.schemas.model.TargetWithOptId
import lucuma.ui.primereact.*
import org.typelevel.log4cats.Logger

trait AsterismModifier:

  protected def insertSiderealTarget(
    programId:        Program.Id,
    obsIds:           ObsIdSet,
    obsAndTargets:    UndoSetter[ObservationsAndTargets],
    targetWithOptId:  TargetWithOptId,
    onAsterismUpdate: OnAsterismUpdateParams => Callback
  )(odbApi: OdbTargetApi[IO])(using FetchClient[IO, ObservationDB]): IO[Unit] =
    targetWithOptId match
      case TargetWithOptId(oTargetId, target @ Target.Sidereal(_, _, _, _)) =>
        oTargetId
          .fold(
            odbApi
              .insertTarget(programId, target)
              .map((_, true))
          )(id => IO((id, false)))
          .flatMap((id, created) =>
            (AsterismActions
              .addTargetToAsterisms(
                TargetWithId(id, targetWithOptId.target),
                obsIds,
                created,
                onAsterismUpdate
              )
              .set(obsAndTargets)(false) >>
              // Do the first onAsterismUpdate here so it is synchronous with the setter in the Action.
              // the ".async.toCallback" seems to let the model update before we try changing the UI
              onAsterismUpdate(
                OnAsterismUpdateParams(id, obsIds, true, true)
              ).async.toCallback).toAsync
          )
      case _                                                                =>
        IO.unit

  def targetSelectionPopup(
    label:            String,
    programId:        Program.Id,
    obsIds:           ObsIdSet,
    obsAndTargets:    UndoSetter[ObservationsAndTargets],
    adding:           View[AreAdding],
    onAsterismUpdate: OnAsterismUpdateParams => Callback,
    readOnly:         Boolean = false,
    buttonClass:      Css = Css.Empty
  )(using odbApi: OdbTargetApi[IO])(using
    FetchClient[IO, ObservationDB],
    Logger[IO]
  ): TargetSelectionPopup =
    TargetSelectionPopup(
      "Add Target",
      TargetSource.FromProgram[IO](programId) :: TargetSource.forAllCatalogs[IO],
      selectExistingLabel = "Link",
      selectExistingIcon = Icons.Link,
      selectNewLabel = label,
      selectNewIcon = Icons.New,
      trigger = Button(
        severity = Button.Severity.Success,
        icon = Icons.New,
        disabled = readOnly || adding.get.value,
        loading = adding.get.value,
        label = label,
        clazz = buttonClass |+| ExploreStyles.Hidden.when_(readOnly)
      ).tiny.compact,
      onSelected = targetWithOptId =>
        insertSiderealTarget(
          programId,
          obsIds,
          obsAndTargets,
          targetWithOptId,
          onAsterismUpdate
        )(odbApi).switching(adding.async, AreAdding(_)).runAsync
    )
