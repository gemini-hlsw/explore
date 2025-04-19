// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.enums

import lucuma.core.util.Enumerated

enum GridLayoutSection(val value: String) derives Enumerated:
  case ProgramsLayout              extends GridLayoutSection("programs")
  case ObservationsLayout          extends GridLayoutSection("observations")
  case ObservationsSpecPhotoLayout extends GridLayoutSection("observations_specphoto")
  case ObservationsTwilightLayout  extends GridLayoutSection("observations_twilight")
  case ObservationsSequenceLayout  extends GridLayoutSection("observations_sequence")
  case ObservationListLayout       extends GridLayoutSection("observation_list")
  case TargetLayout                extends GridLayoutSection("targets")
  case ConstraintsLayout           extends GridLayoutSection("constraints")
  case SchedulingLayout            extends GridLayoutSection("scheduling")
  case OverviewLayout              extends GridLayoutSection("overview")
  case ProposalLayout              extends GridLayoutSection("proposal")
  case GroupEditLayout             extends GridLayoutSection("groupedit")

  private val tag = value
