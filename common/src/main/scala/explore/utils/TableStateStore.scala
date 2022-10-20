// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.utils

import cats.Endo
import lucuma.react.table.TableState

trait TableStateStore[F[_]]:
  def load(): F[Endo[TableState]]
  def save(state: TableState): F[Unit]
