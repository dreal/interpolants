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
  
  def ite(a: Formula, b: Formula, c: Formula) = {
    And(Or(Not(a), b), Or(a, c))
  }


}

object RealLit {
  def unapply(f: Formula): Option[Double] = f match {
    case Literal(d: Double) => Some(d)
    case Literal(d: Float)  => Some(d.toDouble)
    case Literal(d: Long)   => Some(d.toDouble)
    case Literal(d: Int)    => Some(d.toDouble)
    case Literal(d: Short)  => Some(d.toDouble)
    case Literal(d: Byte)   => Some(d.toDouble)
    case _                  => None
  }
}

object LowerBound {
  def unapply(f: Formula): Option[(Variable,Double)] = f match {
    //non-strict var first
    case Geq(   v @ Variable(_), RealLit(d) ) => Some(v -> d)
    case Not(Lt(v @ Variable(_), RealLit(d) )) => Some(v -> d)
    //non-strict var second
    case Leq(   RealLit(d), v @ Variable(_)  ) => Some(v -> d)
    case Not(Gt(RealLit(d), v @ Variable(_) )) => Some(v -> d)
    //strict var fist
    case Gt(     v @ Variable(_), RealLit(d)   ) => Some(v -> d)
    case Not(Leq(v @ Variable(_), RealLit(d)  )) => Some(v -> d)
    //strict var second
    case Lt(     RealLit(d), v @ Variable(_)   ) => Some(v -> d)
    case Not(Geq(RealLit(d), v @ Variable(_)  )) => Some(v -> d)
    case _ => None
  }
}

object UpperBound {
  def unapply(f: Formula): Option[(Variable,Double)] = f match {
    //non-strict
    case Leq(v @ Variable(_), RealLit(d)) => Some(v -> d)
    case Geq(RealLit(d), v @ Variable(_)) => Some(v -> d)
    case Not(Gt(v @ Variable(_), RealLit(d))) => Some(v -> d)
    case Not(Lt(RealLit(d), v @ Variable(_))) => Some(v -> d)
    //strict
    case Lt(v @ Variable(_), RealLit(d)) => Some(v -> d)
    case Gt(RealLit(d), v @ Variable(_)) => Some(v -> d)
    case Not(Leq(RealLit(d), v @ Variable(_))) => Some(v -> d)
    case Not(Geq(v @ Variable(_), RealLit(d))) => Some(v -> d)
    case _ => None
  }
}

