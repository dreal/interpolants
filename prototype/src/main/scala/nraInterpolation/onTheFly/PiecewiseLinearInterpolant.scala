package nraInterpolation.onTheFly

import nraInterpolation.Utils._
import nraInterpolation.Side._
import ProofEvent._
import dzufferey.smtlib._
import java.io._


class PiecewiseLinearInterpolant(labels: Map[Formula, Side]) {
  
  val aFomula = labels.filter(_._2 == A).map(_._1)
  val bFomula = labels.filter(_._2 == B).map(_._1)

  val aVariables = aFomula.flatMap(_.freeVariables).toSet
  val bVariables = bFomula.flatMap(_.freeVariables).toSet
  val commonVariables = aVariables intersect bVariables
  
  //the proof is of the form: domain (split | contract | conflict)*
  //optionally take the actual domain
  def extractFromProof(reader: BufferedReader, actualDomain: Option[Values] = None): (Values, Formula) = {
    // stores the domains currently explored
    val domains = scala.collection.mutable.Stack[Values]()
    // the stack stores the branching points and partial interpolants
    val stack = scala.collection.mutable.Stack[(Variable, Double, Boolean, Option[Formula])]()
    // the result
    var i: Option[Formula] = None
    // push/pop the stack when we have a new partial interpolant
    def pushPartialI(f: Formula) {
      if (stack.isEmpty) {
        assert(i.isEmpty)
        i = Some(f)
      } else {
        val (v, pivot, smallerFirst, part1) = stack.pop
        domains.pop
        part1 match { //is the partial interpolant for the left branch of the Split already computed ?
          case Some(i1) =>
            val (smaller, larger) = if (smallerFirst) (i1, f) else (f, i1)
            val i = if (commonVariables(v)) ite(Leq(v, Literal(pivot)), smaller, larger)
                    else if (aVariables(v)) Or(smaller, larger)
                    else if (bVariables(v)) And(smaller, larger)
                    else sys.error("unknow variable")
            pushPartialI(i)
          case None =>
            stack.push((v, pivot, smallerFirst, Some(f)))
        }
      }
    }
    def onEvent(e: ProofEvent): Unit = e match {
      case Domain(domain) =>
        actualDomain match {
          case Some(d) =>
            domains.push(d)
            //check that we don't have a smaller domain than actualDomain
            d.foreach{ case (v, (l1,u1)) =>
              val (l2, u2) = domain(v)
              assert(l2 <= l1)
              assert(u1 <= u2)
            }
          case None =>
            domains.push(domain)
        }
      case Conflict(f) =>
        val i = labels.get(f) match {
          case Some(A) => False()
          case Some(B) => True()
          case _ => sys.error("conflict: '"+f+"' not part of A or B ???")
        }
        pushPartialI(i)
      case Split(v, pivot, smallerFirst) =>
        stack.push((v, pivot, smallerFirst, None))
        val d = domains.top
        val (lb, ub) = d(v)
        val dSmall = d + (v -> (lb, pivot))
        val dLarge = d + (v -> (pivot, ub))
        if (smallerFirst) {
          domains.push(dLarge)
          domains.push(dSmall)
        } else {
          domains.push(dSmall)
          domains.push(dLarge)
        }
      case Contraction(f, domain) =>
        //reduce to Split and Conflict
        val precondition: Values = domains.top
        val c = Conflict(f)
        domain.foreach{ case(v, (lb, ub)) =>
          val (plb, pub) = precondition(v)
          //pruning the lower part: [plb, lb]
          if (plb < lb) {
            onEvent(Split(v, lb, true))
            onEvent(c)
          }
          //pruning the upper part: [ub, pub]
          if (pub > ub) {
            onEvent(Split(v, ub, false))
            onEvent(c)
          }
        }
    }

    Parser.parseAllSteps(onEvent, reader)
    assert(i.isDefined)
    assert(stack.isEmpty)
    assert(domains.size == 1)
    (domains.pop, FormulaUtils.simplifyBool(FormulaUtils.nnf(FormulaUtils.normalize(i.get))))
  }

}
