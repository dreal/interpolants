
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

  def toSplit: ProofStep = {
    if (next.precondition == null) {
      next.precondition = after
    }
    pruned.foldLeft(next)( (acc, p) => {
      val (v, (lb, ub)) = p
      val c = Conflict(f)
      c.precondition = acc.precondition + p
      val s = Split(v, acc, c)
      s.precondition = acc.precondition + (v -> precondition(v))
      s
    })
  }

}

case class Split(v: Variable, left: ProofStep, right: ProofStep) extends ProofStep {

  def splitAt: Double = {
    val (ll, lu) = left.precondition(v)
    val (rl, ru) = right.precondition(v)
    if (lu == rl) rl
    else if (ll == ru) ru
    else sys.error("not sure where the split happended")
  }

}

object ProofStep {

  protected def prettyPrint(p: ProofStep, indent: Int) {
    for (_ <- 0 until indent) print(" ")
    p match {
      case Conflict(f) => println("conflict with " + Printer.toString(f))
      case c @ Contraction(f, a, next) =>
        println("contraction with " + Printer.toString(f))
        a.foreach{ case (v,(l,u)) =>
          for (_ <- 0 until indent) print(" ")
          println(v + " ∈ [" + l + ", " + u + "]")
        }
        c.pruned.foreach{ case (v,(l,u)) =>
          for (_ <- 0 until indent) print(" ")
          println("¬" + v + " ∈ [" + l + ", " + u + "]")
        }
        prettyPrint(next, indent + 2)
      case Split(v, l, r) =>
        println("split on " + v)
        prettyPrint(l, indent + 2)
        prettyPrint(r, indent + 2)
    }
  }

  def prettyPrint(p: ProofStep) {  prettyPrint(p, 0) }

}
