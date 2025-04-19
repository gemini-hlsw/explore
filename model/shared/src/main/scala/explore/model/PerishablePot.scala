// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import crystal.Pot

type PerishablePot[A] = Pot[Perishable[A]]

object PerishablePot:
  def apply[A](a: A): PerishablePot[A]         = Pot(Perishable(a))
  def error[A](e: Throwable): PerishablePot[A] = Pot.error(e)

  extension [A](pot: PerishablePot[A])
    def isStale: Boolean                                = pot.fold(false, _ => false, _.isStale)
    def isStaleOrPending: Boolean                       = pot.fold(true, _ => true, _.isStale)
    def setStale: Pot[Perishable[A]]                    = pot.map(_.setStale)
    def mapPerishable[B](f: A => B): Pot[Perishable[B]] = pot.map(_.map(f))
    def asValuePot: Pot[A]                              = pot.map(_.value)
    def error: Option[Throwable]                        = pot.fold(None, Some(_), _ => None)
