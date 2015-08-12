package nraInterpolation

import dzufferey.smtlib._
import math._

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
      val (l,u) = acc.precondition(v)
      s.precondition = acc.precondition + (v -> (min(l,lb),max(u,ub)))
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
    else sys.error( "not sure where the split happended: "+v+"\n"+
                    precondition+"\n"+
                    left.precondition+"\n"+
                    right.precondition+"\n")
  }

  def smallFirst: (ProofStep, ProofStep) = {
    val (ll, lu) = left.precondition(v)
    val (rl, ru) = right.precondition(v)
    if (lu == rl) (left, right)
    else if (ll == ru) (right, left)
    else sys.error("not sure where the split happended")
  }

  def smaller: ProofStep = smallFirst._1
  
  def larger: ProofStep = smallFirst._2

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

  /* if dReal took a bound as a constraint, remove it from the proof. */
  def removeBounds(bnds: Map[Variable,(Double,Double)], prf: ProofStep): ProofStep = {
    def tighten(b: Map[Variable,(Double,Double)]): Map[Variable,(Double,Double)] = {
      b.keySet.foldLeft(b)( (acc, v) => {
        val (l1,u1) = acc(v)
        val (l2,u2) = bnds(v)
        if (l2 > l1 || u2 < u1) acc + (v -> (max(l1,l2), min(u1,u2)))
        else acc
      })
    }
    def tightenBounds(prf: ProofStep) {
      if (prf.precondition == null) {
        println("no bounds: " + prf)
      } else {
        prf.precondition = tighten(prf.precondition)
      }
    }
    def process(prf: ProofStep): ProofStep = {
      tightenBounds(prf)
      prf match {
        case c @ Conflict(_) => c
        case s @ Split(v, l, r) =>
          val l2 = process(l)
          val r2 = process(r)
          val (ll,lu) = l2.precondition(v)
          if (ll == lu) {
            r2
          } else {
            val (rl,ru) = r2.precondition(v)
            if (rl == ru) l2
            else {
              val s2 = Split(v, l2, r2)
              s2.precondition = s.precondition
              s2
            }
          }
        case c @ Contraction(f, after, next) => 
          if (next.precondition == null) {
            next.precondition = after
          }
          val next2 = process(next)
          val after2 = tighten(after)
          if (c.precondition == after2) next2
          else {
            val c2 = Contraction(f, after2, next2)
            c2.precondition = c.precondition
            c2
          }
      }
    }
    if (prf.precondition == null) {
      prf.precondition = bnds
    }
    process(prf)
  }

  def toSplit(prf: ProofStep): ProofStep = prf match {
    case c @ Conflict(_) => c
    case s @ Split(v, l, r) =>
      val s2 = Split(v, toSplit(l), toSplit(r))
      s2.precondition = s.precondition
      s2
    case c @ Contraction(f, after, next) => 
      val next2 = toSplit(next)
      val c2 = Contraction(f, after, next2)
      c2.precondition = c.precondition
      c2.toSplit
  }

}
