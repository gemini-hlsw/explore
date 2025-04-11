// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq

// A value, A, that can be marked as stale or errored.
opaque type Perishable[A] = (A, Boolean)

object Perishable:
  // it is fresh when created
  def apply[A](a: A): Perishable[A] = (a, false)
  given [A: Eq]: Eq[Perishable[A]]  = Eq.by(e => (e._1, e._2))

  extension [A](entry: Perishable[A])
    def value: A                              = entry._1
    def isStale: Boolean                      = entry._2
    def map[B](f:      A => B): Perishable[B] = (f(value), entry._2)
    def mapValue[B](f: A => B): B             = f(value)
    def setStale: Perishable[A]               = (value, true)
