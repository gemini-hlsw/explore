// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.utils

import reactST.{tanstackTableCore => raw}

trait TableStateStore[F[_]]:
  def load(): F[raw.mod.TableState => raw.mod.TableState]
  def save(state: raw.mod.TableState): F[Unit]
