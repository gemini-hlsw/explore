// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.enums

import cats.Eq

enum SelectedPanel:
  case Uninitialized, Tree, Summary, Editor
  def rightPanelVisible: Boolean = this match
    case Uninitialized => false
    case Tree          => false
    case Summary       => true
    case Editor        => true

  def leftPanelVisible: Boolean = !rightPanelVisible

object SelectedPanel:
  given Eq[SelectedPanel] = Eq.fromUniversalEquals
