// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.enums

import lucuma.core.util.Enumerated

/**
 * Enum to give an id to each table
 */
enum TableId(val tag: String) derives Enumerated:
  case ObservationsSummary    extends TableId("observations_summary")
  case ConstraintsSummary     extends TableId("constraints_summary")
  case TargetsSummary         extends TableId("targets_summary")
  case AsterismTargets        extends TableId("asterism_targets")
  case SpectroscopyModes      extends TableId("spectroscopy_modes")
  case ObservationValidations extends TableId("observation_validations")
  case RequestedConfigs       extends TableId("requested_configs")
  case UnrequestedConfigs     extends TableId("unrequested_configs")
  case GroupWarnings          extends TableId("group_warnings")
