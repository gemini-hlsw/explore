// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLSubquery
import clue.annotation.GraphQL
import explore.model.Observation
import lucuma.schemas.ObservationDB
import lucuma.schemas.odb.*

@GraphQL
object ObservationSubquery extends GraphQLSubquery.Typed[ObservationDB, Observation]("Observation"):

  override val subquery: String = s"""
        {
          id
          title
          subtitle
          observationTime
          observationDuration $TimeSpanSubquery
          posAngleConstraint $PosAngleConstraintSubquery
          targetEnvironment {
            asterism {
              id
            }
            guideTargetName
          }
          constraintSet $ConstraintSetSubquery
          timingWindows $TimingWindowSubquery
          attachments {
            id
          }
          scienceRequirements {
            exposureTimeMode {
              signalToNoise {
                value
                at $WavelengthSubquery
              }
              timeAndCount {
                time $TimeSpanSubquery
                count
                at $WavelengthSubquery
              }
            }
            spectroscopy {
              wavelength $WavelengthSubquery
              resolution
              wavelengthCoverage $WavelengthDeltaSubquery
              focalPlane
              focalPlaneAngle $AngleSubquery
              capability
            }
          }
          observingMode $ObservingModeSubquery
          observerNotes
          calibrationRole
          scienceBand
          configuration $ConfigurationSubquery
          workflow $ObservationWorkflowSubquery
          groupId
          groupIndex
          reference {
            label
          }
        }
      """
