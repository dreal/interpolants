package nraInterpolation.onTheFly

import dzufferey.smtlib._
import dzufferey.utils._
import dzufferey.utils.LogLevel._
import scala.util.parsing.combinator.RegexParsers
import java.io._
import nraInterpolation.Utils._

import ProofEvent._


object Parser extends RegexParsers {

  def nonWhite: Parser[String] = """[^\s,\[\]]+""".r ^^ { _.toString }

  def nonEq: Parser[String] = """[^=]+""".r ^^ { _.toString }

  def number: Parser[String] = """-?(\d+(\.\d*)?)([eE][+-]?\d+)?""".r

  def num: Parser[Double] = (
    number ^^ ( _.toDouble )
  | "-INFTY" ^^^ Double.NegativeInfinity
  | "INFTY" ^^^ Double.PositiveInfinity
  | "-inf" ^^^ Double.NegativeInfinity
  | "inf" ^^^ Double.PositiveInfinity
  )

  def variable: Parser[Variable] = nonWhite ^^ ( id => Variable(id).setType(Real) )

  def range: Parser[(Variable, Double, Double)] = (
    variable ~ (":" ~> nonEq ~> "=" ~> "[" ~> num) ~ ("," ~> num <~ "]") ^^ { case id ~ lb ~ ub => (id, lb, ub) }
  | variable <~ ":" <~ nonEq <~ "=" <~ "[ -INFTY ]" ^^ { case id => (id, Double.NegativeInfinity, Double.NegativeInfinity) }
  | variable <~ ":" <~ nonEq <~ "=" <~ "[ INFTY ]" ^^ { case id => (id, Double.PositiveInfinity, Double.PositiveInfinity) }
  | variable <~ ":" <~ nonEq <~ "=" <~ "[ ENTIRE ]" ^^ { case id => (id, Double.NegativeInfinity, Double.PositiveInfinity) }
  )

  def ranges: Parser[Map[Variable, (Double, Double)]] = rep(range) ^^ ( _.foldLeft(Map[Variable, (Double, Double)]())( (acc, k) => acc + (k._1 -> (k._2, k._3))))

  //polarity = preceded by '!'
  def formula: Parser[Formula] = opt("!") ~ """.*""".r ^^ { case a ~ b => val f = parseFormula(b)
                                                                          val g = if (a.isDefined) Not(f) else f
                                                                          FormulaUtils.nnf(FormulaUtils.normalize(g)) }

  def domain: Parser[Domain] =
    "[domain]" ~> ranges ^^ ( Domain(_) )

  def contraction: Parser[Contraction] =
    "[after pruning] by" ~> formula ~ ranges ^^ { case f ~ p => Contraction(f, p) }

  def conflict: Parser[Conflict] =
    "[conflict detected] by" ~> formula ^^ ( Conflict(_) )

  def split: Parser[Split] =
    "[branching] on" ~> formula ^^ { case Leq(v @ Variable(_), Literal(d: Double)) => Split(v, d, true)
                                     case Leq(v @ Variable(_), Literal(d: Long)) => Split(v, d.toDouble, true)
                                     case Geq(v @ Variable(_), Literal(d: Double)) => Split(v, d, false)
                                     case Geq(v @ Variable(_), Literal(d: Long)) => Split(v, d.toDouble, false)
                                     case Not(Lt(v @ Variable(_), Literal(d: Double))) => Split(v, d, false)
                                     case Not(Lt(v @ Variable(_), Literal(d: Long))) => Split(v, d.toDouble, false)
                                     case Not(Lt(Literal(d: Double), v @ Variable(_))) => Split(v, d, true)
                                     case Not(Lt(Literal(d: Long), v @ Variable(_))) => Split(v, d.toDouble, true)
                                     case other => sys.error("branching, found " + other.toStringFull) }

  def event: Parser[ProofEvent] = domain | split | contraction | conflict

  def proof = rep(event)

  def parseAllSteps(str: String) = {
    val result = parseAll(proof, str)
    if (result.successful) {
      result.get
    } else {
      sys.error("could not parse:\n" + str)
    }

  }
  
  def parseAllSteps(onEvent: ProofEvent => Unit, reader: BufferedReader) {
    val buffer = new StringBuilder()
    while(reader.ready) {
      val line = reader.readLine
      if (line.startsWith("[")) {
        if (!buffer.isEmpty) {
          val step = parse(event, buffer.toString)
          if (step.successful) onEvent(step.get)
        }
        buffer.clear
      }
      buffer.append(line)
      buffer.append("\n")
    }
    if (!buffer.isEmpty) {
      val step = parse(event, buffer.toString)
      if (step.successful) onEvent(step.get)
    }
  }

}
