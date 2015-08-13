package nraInterpolation

import dzufferey.smtlib._
import dzufferey.utils._
import Utils._
import java.io.{FileReader, BufferedReader}

class Interpolate(val a: Formula, val b: Formula, val domain: Map[Variable, (Double,Double)]) {
  
  lazy val i: Formula = computeInterpolant
  def interpolant = i

  val aVariables = a.freeVariables
  val bVariables = b.freeVariables
  val commonVariables = aVariables intersect bVariables

  protected def querySolver(f: Formula, delta: Double, proof: Boolean = false, file: Option[String] = None) = {
    val arg = if (proof) Array("--in","--model","--readable-proof","--no-simp") else Array[String]("--in","--model")
    val solver = new DRealHack(QF_NRA, Interpolate.solverCmd, arg, Some(delta), true, false, file)
    val f2 = FormulaUtils.nnf(f)
    fixTypes(f2)
    f2.freeVariables.foreach( v => {
      val (lb, ub) = domain(v)
      solver.assert(Geq(v, Literal(lb)))
      solver.assert(Leq(v, Literal(ub)))
    })
    solver.assert(f2)
    solver.checkSat(100000) /* 100 sec */
  }
    
  //weakened formula
  val a0 = if (Interpolate.w > 0.0) weaken(a, Interpolate.w) else a
  val b0 = if (Interpolate.w > 0.0) weaken(b, Interpolate.w) else b
  
  //label clause by side
  val labeling = {
    val ca = FormulaUtils.getConjuncts(a0).map( _ -> Side.A )
    val cb = FormulaUtils.getConjuncts(b0).map( _ -> Side.B )
    Map[Formula, Side.Side]() ++ ca ++ cb
  }

  protected def getProof: Option[String] = {
    //get a proof of unsat
    val f = And(a0, b0)
    
    querySolver(f, Interpolate.delta, true) match {
      case UnSat => Some("output.proof")
      case Sat(_) => sys.error("sat")
      case other => sys.error("not expected: " + other)
    }
  }

  def computeInterpolant = {
    getProof match {
      case Some(fileName) =>
        val proofTrace = IO.readTextFile(fileName)
        val rawProof = ProofParser.parse(proofTrace)
        SysCmd(Array("rm", fileName))
        println("proof:")
        ProofStep.prettyPrint(rawProof)
        val proof = ProofStep.toSplit(ProofStep.removeBounds(domain, rawProof))
        val mkInterpolant = new PiecewiseLinearInterpolant(proof, labeling)
        val i = mkInterpolant.interpolant
        i
      case None => sys.error("Could not get a proof")
    }
  }


  def check = {
    val incl = i.freeVariables.subsetOf(commonVariables)
    if (!incl) println("variables in I not in the intersection.")
    val aCond = querySolver(And(a, Not(i)), Interpolate.deltaCheck) match {
      case UnSat => true
      case other =>
        println("A ∧ ¬I is not unsat: " + other)
        false
    } 
    val bCond = querySolver(And(i, b), Interpolate.deltaCheck) match {
      case UnSat => true
      case other =>
        println("I ∧ B is not unsat: " + other)
        false
    }
    incl && aCond && bCond
  }

  def hyperCubes = {
    val init = domain.filter(kv => commonVariables(kv._1))
    PiecewiseLinearInterpolant.hyperCubes(init, i)
  }

}

class OnTheFlyInterpolate(_a: Formula, _b: Formula, d: Map[Variable, (Double,Double)]) extends Interpolate(_a, _b, d) {

  override def computeInterpolant = {
    getProof match {
      case Some(fileName) =>
        val proofTrace = IO.readTextFile(fileName)
        val mkInterpolant = new onTheFly.PiecewiseLinearInterpolant(labeling)
        val reader = new BufferedReader(new FileReader(fileName))
        val (_, i) = mkInterpolant.extractFromProof(reader, Some(domain))
        reader.close
        SysCmd(Array("rm", fileName))
        i
      case None => sys.error("Could not get a proof")
    }
  }

}

object Interpolate extends dzufferey.arg.Options {

  def interpolate(a: Formula, b: Formula) {
    val itp = new Interpolate(a, b, getDomain)
    //val itp = new OnTheFlyInterpolate(a, b, getDomain)
    println("interpolant:\n  " + itp.i)
    itp.check
    val cubes = itp.hyperCubes
    println("cubes:")
    cubes.foreach( m => println(m.map{ case (v, (lb, ub)) => v + " ∈ [" + lb + ", " + ub +"]"}.mkString("  ", ", ","")))
  }

  def getDomain: Map[Variable,(Double,Double)] = (lb,ub,domain) match {
    case (Some(l), Some(u), None) =>
      val vs = a.get.freeVariables ++ b.get.freeVariables
      vs.foldLeft(Map.empty[Variable,(Double,Double)])( (acc, v) => acc + (v -> (l, u)) )
    case (None, None, Some(f)) =>
      val vs = a.get.freeVariables ++ b.get.freeVariables
      val formula = parseFormulaFromFile(f)
      val init = vs.map(v => v -> (Double.NegativeInfinity, Double.PositiveInfinity)).toMap
      FormulaUtils.getConjuncts(formula).foldLeft(init)( (acc, f) => f match {
        case LowerBound(v, b) =>
          val (l, u) = acc(v)
          acc + (v -> (math.max(l, b), u))
        case UpperBound(v, b) =>
          val (l, u) = acc(v)
          acc + (v -> (l, math.min(u, b)))
        case other =>
          sys.error("not a bound: " + other)
      })
    case other => sys.error("wrong combination for bound: " + other)
  }

  var solverCmd = "dReal"
  newOption("-s", dzufferey.arg.String( s => solverCmd = s), "solver command (default: dReal)")

  var w = 0.0
  newOption("-w", dzufferey.arg.Real( s => w = s), "weakening before building the proof (default: 0.0)")

  var lb: Option[Double] = None
  var ub: Option[Double] = None
  var domain: Option[String] = None
  newOption("-lb", dzufferey.arg.Real( s => lb = Some(s)), "lower bound")
  newOption("-ub", dzufferey.arg.Real( s => ub = Some(s)), "upper bound")
  newOption("-domain", dzufferey.arg.String( s => domain = Some(s)), "file containing the domain")

  var a: Option[Formula] = None
  var b: Option[Formula] = None
  newOption("-a", dzufferey.arg.String( s => a = Some(parseFormula(s))), "formula A")
  newOption("-b", dzufferey.arg.String( s => b = Some(parseFormula(s))), "formula B")
  newOption("-fa", dzufferey.arg.String( s => a = Some(parseFormulaFromFile(s))), "file containing the formula A")
  newOption("-fb", dzufferey.arg.String( s => b = Some(parseFormulaFromFile(s))), "file containing the formula B")
  
  var delta  = 0.001
  newOption("-d", dzufferey.arg.Real( r => delta = r), "delta (default: 0.001)")
  
  var deltaCheck  = 0.001
  newOption("-dc", dzufferey.arg.Real( r => deltaCheck = r), "delta for checking (default: 0.001)")

  newOption("-v", dzufferey.arg.Unit(() => Logger.moreVerbose), "increase the verbosity level.")
  newOption("-q", dzufferey.arg.Unit(() => Logger.lessVerbose), "decrease the verbosity level.")

  val usage = "-a [formula] -b [formula]"

  def main(args: Array[String]) {
    Logger.disallow("Typer")
    try {
      apply(args)
      assert(a.isDefined, "a undefined")
      assert(b.isDefined, "b undefined")
      interpolate(a.get, b.get)
    } catch { case t: Throwable =>
      Console.err.println("failed to compute an interpolant: " + t + "\n  " + t.getStackTrace.mkString("\n  "))
      sys.exit(-1)
    } finally {
      Solver.executor.shutdownNow
    }
  }

}
