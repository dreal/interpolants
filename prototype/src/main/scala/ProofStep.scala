
import dzufferey.smtlib._

sealed abstract class ProofStep {
  var precondition: Map[Variable, (Double, Double)] = null
}

case class Conflict(f: Formula) extends ProofStep

case class Contraction(f: Formula, after: Map[Variable, (Double, Double)], next: ProofStep) extends ProofStep {

  def pruned: Iterable[(Variable, (Double, Double))] = {
    assert(precondition != null, "requires precondition")
    assert(after.keySet == precondition.keySet)
    var acc = List[(Variable, (Double, Double))]()
    after.foreach{ case(id, (lb, ub)) =>
      val (plb, pub) = precondition(id)
      if (plb != lb) {
        acc ::= (id -> (plb, lb))
      }
      if (pub != ub) {
        acc ::= (id -> (ub, pub))
      }
      acc
    }
    assert(!acc.isEmpty)
    acc
  }

}

case class Split(v: Variable, left: ProofStep, right: ProofStep) extends ProofStep
