// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.givens

import cats.Endo
import cats.Monoid
import cats.MonoidK

given [A]: Monoid[Endo[A]] = MonoidK[Endo].algebra[A]
