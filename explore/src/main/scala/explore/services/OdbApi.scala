// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.services

trait OdbApi[F[_]] extends OdbTargetApi[F] with OdbAsterismApi[F] with OdbProgramApi[F]
