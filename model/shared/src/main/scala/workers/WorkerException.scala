// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package workers

import boopickle.DefaultBasic.*

case class WorkerException(
  originalClass:   String,
  originalMessage: String,
  stackTrace:      List[String]
) extends Exception {
  override val getMessage: String =
    s"$originalClass: $originalMessage\n  " + stackTrace.mkString("\n  ")
}

object WorkerException {
  def fromThrowable(t: Throwable): WorkerException =
    WorkerException(t.getClass.getName, t.getMessage, t.getStackTrace.toList.map(_.toString))

  given Pickler[WorkerException] = generatePickler
}
