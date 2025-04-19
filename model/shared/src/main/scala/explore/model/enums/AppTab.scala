// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.enums

import lucuma.core.util.Enumerated

/**
 * Describes the application tab buttons in the sidebar
 *
 * @param title
 *   The text for the button
 * @param buttonGroup
 *   Groups the buttons with the same value together
 *
 * Within a button group, order is determined by the AppTab Order instance, which is determined by
 * the order in AppTab.all.
 */
enum AppTab(val title: String, val separatorAfter: Boolean = false) derives Enumerated:
  case Program      extends AppTab("Program", true)
  case Proposal     extends AppTab("Proposal", true)
  case Overview     extends AppTab("Overview", true)
  case Observations extends AppTab("Observations")
  case Targets      extends AppTab("Targets")
  case Constraints  extends AppTab("Constraints")
  case Scheduling   extends AppTab("Scheduling")

  def tag: String = title
