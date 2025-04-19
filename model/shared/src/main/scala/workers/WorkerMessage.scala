// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package workers

import boopickle.DefaultBasic.*
import lucuma.core.util.NewType

import java.util.UUID

private object WorkerProcessId extends NewType[UUID]
private type WorkerProcessId = WorkerProcessId.Type

private object Pickled extends NewType[Array[Byte]]
private type Pickled = Pickled.Type

// Low-level protocol messages, not to be used by clients or servers
private object WorkerMessage:
  sealed trait FromClient
  object FromClient:
    case object ClientReady                                 extends FromClient
    case class Start(id: WorkerProcessId, payload: Pickled) extends FromClient
    case class End(id: WorkerProcessId)                     extends FromClient

  sealed trait FromServer
  object FromServer:
    case object ServerReady                                       extends FromServer
    case class Data(id: WorkerProcessId, payload: Pickled)        extends FromServer
    case class Complete(id: WorkerProcessId)                      extends FromServer
    case class Error(id: WorkerProcessId, error: WorkerException) extends FromServer

  private given Pickler[WorkerProcessId] = transformPickler(WorkerProcessId(_))(_.value)

  private given Pickler[Pickled] = transformPickler(Pickled(_))(_.value)

  private given Pickler[FromClient.ClientReady.type] = generatePickler

  private given Pickler[FromClient.Start] = generatePickler

  private given Pickler[FromClient.End] = generatePickler

  given Pickler[FromClient] = generatePickler

  private given Pickler[FromServer.ServerReady.type] = generatePickler

  private given Pickler[FromServer.Data] = generatePickler

  private given Pickler[FromServer.Complete] = generatePickler

  private given Pickler[FromServer.Error] = generatePickler

  given Pickler[FromServer] = generatePickler
