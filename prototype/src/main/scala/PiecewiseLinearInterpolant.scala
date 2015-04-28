
import dzufferey.smtlib._
import Side._


class PiecewiseLinearInterpolant(proof: ProofStep, labels: Map[Formula, Side]) {
  
  val aFomula = labels.filter(_._2 == A).map(_._1)

  val bFomula = labels.filter(_._2 == B).map(_._1)

  val aVariables = aFomula.flatMap(_.freeVariables).toSet

  val bVariables = bFomula.flatMap(_.freeVariables).toSet

  val commonVariables = aVariables intersect bVariables
  
  protected def ite(a: Formula, b: Formula, c: Formula) = {
    And(Or(Not(a), b), Or(a, c))
  }

  protected def extract(prf: ProofStep): Formula = prf match {
    case Conflict(f) =>
      labels.get(f) match {
        case Some(A) => False()
        case Some(B) => True()
        case _ => sys.error("conflict: '"+f+"' not part of A or B ???")
      }
    case s @ Split(v, left, right) =>
      if (commonVariables(v)) ite(Leq(v, Literal(s.splitAt)), extract(left), extract(right))
      else if (aVariables(v)) Or(extract(left), extract(right))
      else if (bVariables(v)) And(extract(left), extract(right))
      else sys.error("unknow variable")
    case c @ Contraction(f, after, next) => extract(c.toSplit)
  }

  def interpolant: Formula = extract(proof)

}
