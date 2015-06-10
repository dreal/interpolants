
import dzufferey.smtlib._
import dzufferey.utils._
import Utils._

object Side extends Enumeration {
  type Side = Value
  val A, B = Value
}

object Interpolate extends dzufferey.arg.Options {

  def querySolver(f: Formula, delta: Double, proof: Boolean = false, file: Option[String] = None) = {
    val arg = if (proof) Array("--in","--model","--readable-proof","--no-simp") else Array[String]("--in","--model")
    val solver = new DRealHack(QF_NRA, solverCmd, arg, Some(delta), true, false, file)
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
    val aCond = querySolver(And(a, Not(i)), deltaCheck) match {
      case UnSat => true
      case other =>
        println("A ∧ ¬I is not unsat: " + other)
        false
    } 
    val bCond = querySolver(And(i, b), deltaCheck) match {
      case UnSat => true
      case other =>
        println("I ∧ B is not unsat: " + other)
        false
    }
    incl && aCond && bCond
  }

  def interpolate(a: Formula, b: Formula) {
    //get a proof of unsat
    val a0 = if (w > 0.0) weaken(a, w) else a
    val b0 = if (w > 0.0) weaken(b, w) else b
    val f = And(a0, b0)

    querySolver(f, delta, true) match {
      case UnSat => // ok
      case Sat(_) => sys.error("sat")
      case other => sys.error("not expected: " + other)
    }
    val proofTrace = IO.readTextFile("output.proof")
    val proof = ProofParser.parse(proofTrace)
    SysCmd(Array("rm", "output.proof"))

    ProofStep.prettyPrint(proof)

    //label clause by side
    val ca = FormulaUtils.getConjuncts(a0).map( _ -> Side.A )
    val cb = FormulaUtils.getConjuncts(b0).map( _ -> Side.B )
    val labeling = Map[Formula, Side.Side]() ++ ca ++ cb

    //val mkInterpolant = new InterpolationQuery(proof, labeling, delta)
    val mkInterpolant = new PiecewiseLinearInterpolant(proof, labeling)
    val i = mkInterpolant.interpolant
    println("interpolant:\n  " + i)
    check(a, b, i)
    val cubes = mkInterpolant.hyperCubes(lb, ub)
    println("cubes:")
    cubes.foreach( m => println(m.map{ case (v, (lb, ub)) => v + " ∈ [" + lb + ", " + ub +"]"}.mkString("  ", ", ","")))
  }

  var solverCmd = "dReal"
  newOption("-s", dzufferey.arg.String( s => solverCmd = s), "solver command (default: dReal)")

  var w = 0.0
  newOption("-w", dzufferey.arg.Real( s => w = s), "weakening before building the proof")

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
  
  var deltaCheck  = 0.1
  newOption("-dc", dzufferey.arg.Real( r => deltaCheck = r), "delta for checking")

  newOption("-v", dzufferey.arg.Unit(() => Logger.moreVerbose), "increase the verbosity level.")
  newOption("-q", dzufferey.arg.Unit(() => Logger.lessVerbose), "decrease the verbosity level.")

  val usage = "-a f_a -b f_b"

  def main(args: Array[String]) {
    Logger.disallow("Typer")
    try {
      apply(args)
      assert(a.isDefined, "a undefined")
      assert(b.isDefined, "b undefined")
      assert(lb < ub)
      interpolate(a.get, b.get)
    } catch { case t: Throwable =>
      Console.err.println("failed to compute an interpolant: " + t + "\n  " + t.getStackTrace.mkString("\n  "))
      sys.exit(-1)
    } finally {
      Solver.executor.shutdownNow
    }
  }

}
