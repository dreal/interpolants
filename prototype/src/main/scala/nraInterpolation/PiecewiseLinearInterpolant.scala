package nraInterpolation

import dzufferey.smtlib._
import Utils._
import Side._


class PiecewiseLinearInterpolant(proof: ProofStep, labels: Map[Formula, Side]) {
  
  val aFomula = labels.filter(_._2 == A).map(_._1)
  val bFomula = labels.filter(_._2 == B).map(_._1)

  val aVariables = aFomula.flatMap(_.freeVariables).toSet
  val bVariables = bFomula.flatMap(_.freeVariables).toSet
  val commonVariables = aVariables intersect bVariables
  
  protected def extract(prf: ProofStep): Formula = prf match {
    case Conflict(f) =>
      labels.get(f) match {
        case Some(A) => False()
        case Some(B) => True()
        case _ => sys.error("conflict: '"+f+"' not part of A or B ???")
      }
    case s @ Split(v, left, right) =>
      if (commonVariables(v)) ite(Leq(v, Literal(s.splitAt)), extract(s.smaller), extract(s.larger))
      else if (aVariables(v)) Or(extract(left), extract(right))
      else if (bVariables(v)) And(extract(left), extract(right))
      else sys.error("unknow variable")
    case c @ Contraction(f, after, next) => extract(c.toSplit)
  }

  def interpolant: Formula = {
    val f1 = extract(proof)
    val f2 = FormulaUtils.nnf(FormulaUtils.normalize(f1))
    FormulaUtils.simplifyBool(f2)
  }

  def hyperCubes(lb: Double, ub: Double): Seq[Map[Variable,(Double,Double)]] = {
    val init = commonVariables.map(v => v -> (lb, ub)).toMap
    PiecewiseLinearInterpolant.hyperCubes(init, interpolant)
  }
  
  def hyperCubes(domain: Map[Variable,(Double,Double)]): Seq[Map[Variable,(Double,Double)]] = {
    val init = domain.filter(kv => commonVariables(kv._1))
    PiecewiseLinearInterpolant.hyperCubes(init, interpolant)
  }

}

object PiecewiseLinearInterpolant {

  def hyperCubes(commonVariablesDomain: Map[Variable,(Double,Double)],
                 interpolant: Formula): Seq[Map[Variable,(Double,Double)]] = {
    def traverse(f: Formula, acc: Map[Variable,(Double,Double)]): Seq[Map[Variable,(Double,Double)]] = f match {
      case And(args @ _*) =>
        args.foldLeft(Seq(acc))( (acc , a) => acc.flatMap( ac => traverse(a, ac)) )
      case Or(args @ _*) =>
        args.flatMap( a => traverse(a, acc) )
      case Lt(v @ Variable(_), Literal(d: Double)) =>
        val (l, u) = acc(v)
        val m = acc + (v -> (l, math.min(d, u)))
        Seq(m)
      case Not(Lt(Literal(d: Double), v @ Variable(_))) =>
        val (l, u) = acc(v)
        val m = acc + (v -> (l, math.min(d, u)))
        Seq(m)
      case Lt(Literal(d: Double), v @ Variable(_)) =>
        val (l, u) = acc(v)
        val m = acc + (v -> (math.max(d, l), u))
        Seq(m)
      case Not(Lt(Literal(d: Double), v @ Variable(_))) =>
        val (l, u) = acc(v)
        val m = acc + (v -> (math.max(d, l), u))
        Seq(m)
      case other =>
        sys.error("unexpected: " + other)
    }
    val cubes = traverse(interpolant, commonVariablesDomain)
    //filter the empty ones
    cubes.filter( c => c.forall{ case (_, (l, u)) => l < u} )
  }

}
