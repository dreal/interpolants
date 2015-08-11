package nraInterpolation.onTheFly

import dzufferey.smtlib._
import math._

object ProofEvent {
  type Values = Map[Variable, (Double, Double)]
}

import ProofEvent._

sealed abstract class ProofEvent
case class Domain(d: Values) extends ProofEvent
case class Conflict(f: Formula) extends ProofEvent
case class Contraction(f: Formula, after: Values) extends ProofEvent
case class Split(v: Variable, value: Double, smallerFirst: Boolean) extends ProofEvent
