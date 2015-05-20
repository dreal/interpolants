
import dzufferey.smtlib._

import Utils._
import Side._

class InterpolationQuery(proof: ProofStep, labels: Map[Formula, Side], delta: Double) {
    
  val aFomula = labels.filter(_._2 == A).map(_._1)

  val bFomula = labels.filter(_._2 == B).map(_._1)

  val aVariables = aFomula.flatMap(_.freeVariables).toSet

  val bVariables = bFomula.flatMap(_.freeVariables).toSet

  val commonVariables = aVariables intersect bVariables

  protected def squareSolution(m: Map[Variable, Double]) = {
    val lits = m.toList.flatMap{ case (v, d) =>
      List( Lt(Literal(d + delta), v),
            Lt(v, Literal(d - delta)))
    }
    Or(lits: _*)
  }

  //TODO make a query to get some solutions
  //This are partial solutions
  protected def allSat(formula: Formula, ranges: List[Formula]): List[Map[Variable, Double]] = {
    val vs = formula.freeVariables
    val rs = ranges.filter( x => (x.freeVariables -- vs).isEmpty )
    var solutions: List[Map[Variable, Double]] = Nil
    var cstr: List[Formula] = formula :: ranges
    var again = true
    while (again) {
      val solver = DReal(QF_NRA, delta)
      cstr.foreach( c => {
        fixTypes(c)
        solver.assert(c)
      })
      solver.checkSat(100000) match {
        case Sat(Some(model)) =>
          val s = vs.map( v => model(v) match {
            case ValD(d) => v -> d
            case ValI(i) => v -> i.toDouble
            case other => sys.error("expected ValD/ValI, found: " + other)
          }).toMap
          solutions = s :: solutions
          cstr = squareSolution(s) :: cstr
        case UnSat =>
          again = false
        case other =>
          sys.error("not expected: " + other)
      }
    }
    solutions
  }

  def makePoints(c: Contraction): Iterable[Map[Variable, Double]] = {
    def makeBounds(v: Variable, lb: Double, ub: Double) = {
      And(Leq(v, Literal(ub + delta)), Leq(Literal(lb - delta), v))
    }
    def makeRange(vs: Map[Variable, (Double, Double)]): List[Formula] = {
      vs.map{ case (v, (lb, ub)) => makeBounds(v, lb, ub) }.toList
    }
    val default = makeRange(c.precondition)
    c.pruned.flatMap{ case (v, (lb, ub)) =>
      val ranges = makeBounds(v, lb, ub) :: default
      allSat(c.f, ranges)
    }
  }

  def interpolant: Formula = {
    ???
  }

  //TODO printing a scipy rbf query (later try more complex methods)
  // http://docs.scipy.org/doc/scipy/reference/generated/scipy.interpolate.Rbf.html#scipy.interpolate.Rbf
  // wiki.scipy.org/Cookbook/RadialBasisFunctions

  //for quadratic optimization and gradient descent:
  //- http://www.joptimizer.com/
  //- http://commons.apache.org/proper/commons-math/apidocs/org/apache/commons/math3/optim/package-summary.html

/*
to guarantee soundness, can we put it in a loop
    A ∧ ¬I is unsat
    I ∧ B is unsat
if it is sat, we can add the point and redo the interpolation
*/
}
