package nraInterpolation

import dzufferey.smtlib._

object Utils {
  
  def fixTypes(f: Formula) {
    val t = new FormulaUtils.Traverser {
      override def traverse(f: Formula) {
        super.traverse(f)
        if (f.tpe == Int || f.tpe == Wildcard) f.setType(Real)
      }
    }
    t.traverse(f)
  }

  def fixNegativeLiteral(f: Formula) = {
    FormulaUtils.map({
      case Variable(v) if v.startsWith("-") => Literal(-v.substring(1).toDouble)
      case other => other
    }, f)
  }

  def parseFormula(s: String): Formula = {
    dzufferey.smtlib.Parser.parseTerm(s) match {
      case Some(f) =>
        fixTypes(f)
        val f1 = fixNegativeLiteral(f)
        val f2 = FormulaUtils.normalize(f1)
        FormulaUtils.nnf(f2)
      case other => sys.error("expected formula: " + s)
    }
  }
  
  def parseFormulaFromFile(s: String): Formula = {
    val content = dzufferey.utils.IO.readTextFile(s)
    parseFormula(content)
  }

  def weaken(f: Formula, delta: Double): Formula = {
    val d = Literal(delta.abs)
    val f1 = FormulaUtils.map({
      case Eq(a, b) => And(Leq(a, Plus(b, d)), Leq(b, Plus(a, d)))
      case Leq(a, b) => Leq(a, Plus(b, d))
      case Lt(a, b) => Leq(a, Plus(b, d))
      case Geq(a, b) => Geq(Plus(a, d), b)
      case Gt(a, b) => Gt(Plus(a, d), b)
      case a @ And(_*) => And(FormulaUtils.getConjuncts(a):_*)
      case other => other
    }, f)
    FormulaUtils.nnf(FormulaUtils.normalize(f1))
  }

}
