
import dzufferey.smtlib._
import dzufferey.utils._

object Side extends Enumeration {
  type Side = Value
  val A, B = Value
}

object Interpolate extends dzufferey.arg.Options {

  def querySolver(f: Formula, proof: Boolean = false) = {
    val arg = if (proof) Array("-readable_proof") else Array[String]()
    val solver = new DRealHack(QF_NRA, "dReal", arg, Some(delta), true, false, None)
    val f2 = FormulaUtils.nnf(f)
    fixTypes(f2)
    f2.freeVariables.foreach( v => {
      solver.assert(Geq(v, Literal(lb)))
      solver.assert(Leq(v, Literal(ub)))
    })
    solver.assert(f2)
    solver.checkSat(100000) /* 100 sec */
  }

  def check(a: Formula, b: Formula, i: Formula) = {
    val incl = i.freeVariables.subsetOf(a.freeVariables intersect b.freeVariables)
    if (!incl) println("variables in I not in the intersection.")
    val aCond = querySolver(And(a, Not(i))) match {
      case UnSat => true
      case other =>
        println("A ∧ ¬I is not unsat: " + other)
        false
    } 
    val bCond = querySolver(And(i, b)) match {
      case UnSat => true
      case other =>
        println("I ∧ B is not unsat: " + other)
        false
    }
    incl && aCond && bCond
  }

  def interpolate(a: Formula, b: Formula): Formula = {
    //get a proof of unsat
    querySolver(And(a,b), true) match {
      case UnSat => // ok
      case Sat(_) => sys.error("sat")
      case other => sys.error("not expected: " + other)
    }
    val proofTrace = IO.readTextFile("output.proof")
    val proof = ProofParser.parse(proofTrace)
    SysCmd(Array("rm", "output.proof"))

    ProofStep.prettyPrint(proof)

    //label clause by side
    val ca = FormulaUtils.getConjuncts(a).map( _ -> Side.A )
    val cb = FormulaUtils.getConjuncts(b).map( _ -> Side.B )
    val labeling = Map[Formula, Side.Side]() ++ ca ++ cb

    //val mkInterpolant = new InterpolationQuery(proof, labeling, delta)
    val mkInterpolant = new PiecewiseLinearInterpolant(proof, labeling)
    mkInterpolant.interpolant
  }

  var lb = -10.0
  var ub =  10.0
  newOption("-lb", dzufferey.arg.Real( s => lb = s), "lower bound")
  newOption("-ub", dzufferey.arg.Real( s => ub = s), "upper bound")

  var a: Option[Formula] = None
  var b: Option[Formula] = None
  newOption("-a", dzufferey.arg.String( s => a = Some(parseFormula(s))), "")
  newOption("-b", dzufferey.arg.String( s => b = Some(parseFormula(s))), "")
  
  var delta  = 0.1
  newOption("-d", dzufferey.arg.Real( r => delta = r), "delta")
  
  val usage = "-a f_a -b f_b"

  def main(args: Array[String]) {
    try {
      apply(args)
      assert(a.isDefined, "a undefined")
      assert(b.isDefined, "b undefined")
      assert(lb < ub)
      val i = interpolate(a.get, b.get)
      println("interpolant: " + i)
      check(a.get, b.get, i)
    } catch { case t: Throwable =>
      Console.err.println("failed to compute an interpolant: " + t + "\n  " + t.getStackTrace.mkString("\n  "))
      sys.exit(-1)
    } finally {
      Solver.executor.shutdownNow
    }
  }

  def parseFormula(s: String): Formula = {
    dzufferey.smtlib.Parser.parseTerm(s) match {
      case Some(f) =>
        fixTypes(f)
        FormulaUtils.nnf(FormulaUtils.normalize(f))
      case other => sys.error("expected formula: " + other)
    }
  }
  
  def fixTypes(f: Formula) {
    val t = new FormulaUtils.Traverser {
      override def traverse(f: Formula) {
        super.traverse(f)
        if (f.tpe == Int || f.tpe == Wildcard) f.setType(Real)
      }
    }
    t.traverse(f)
  }


}
