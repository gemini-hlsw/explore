// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import lucuma.core.model.Asterism
import lucuma.core.model.Target

package object model {
  type PointingId = Either[Asterism.Id, Target.Id]
}
