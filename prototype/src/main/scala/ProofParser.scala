
import dzufferey.smtlib._
import dzufferey.utils._
import dzufferey.utils.LogLevel._
import java.io._

object ProofParser extends scala.util.parsing.combinator.RegexParsers {

  def nonWhite: Parser[String] = """[^\s,\[\]]+""".r ^^ { _.toString }

  def nonEq: Parser[String] = """[^=]+""".r ^^ { _.toString }

  def number: Parser[String] = """-?(\d+(\.\d*)?)([eE][+-]?\d+)?""".r

  def num: Parser[Double] = (
    number ^^ ( _.toDouble )
  | "-INFTY" ^^^ Double.NegativeInfinity
  | "INFTY" ^^^ Double.PositiveInfinity
  )

  def variable: Parser[Variable] = nonWhite ^^ ( id => Variable(id).setType(Real) )

  def range: Parser[(Variable, Double, Double)] = (
    variable ~ (":" ~> nonEq ~> "=" ~> "[" ~> num) ~ ("," ~> num <~ "]") ^^ { case id ~ lb ~ ub => (id, lb, ub) }
  | variable <~ ":" <~ nonEq <~ "=" <~ "[ -INFTY ]" ^^ { case id => (id, Double.NegativeInfinity, Double.NegativeInfinity) }
  | variable <~ ":" <~ nonEq <~ "=" <~ "[ INFTY ]" ^^ { case id => (id, Double.PositiveInfinity, Double.PositiveInfinity) }
  | variable <~ ":" <~ nonEq <~ "=" <~ "[ ENTIRE ]" ^^ { case id => (id, Double.NegativeInfinity, Double.PositiveInfinity) }
  )

  def ranges: Parser[Map[Variable, (Double, Double)]] = rep(range) ^^ ( _.foldLeft(Map[Variable, (Double, Double)]())( (acc, k) => acc + (k._1 -> (k._2, k._3))))

  //TODO polarity preceded by '!'
  def formula: Parser[Formula] = opt("!") ~ """.*""".r ^^ { case a ~ b => val f = Interpolate.parseFormula(b)
                                                                          val g = if (a.isDefined) Not(f) else f
                                                                          FormulaUtils.nnf(FormulaUtils.normalize(g)) }

  def contraction: Parser[Contraction] =
    "[after pruning] by " ~> formula ~ ranges ~ proof ^^ { case f ~ r2 ~ p => Contraction(f, r2, p) }

  def conflict: Parser[Conflict] =
    "[conflict detected] by " ~> formula ^^ ( Conflict(_) )

  def step: Parser[ProofStep] = (
    "[before pruning]" ~> ranges ~ (contraction | conflict) ^^ { case r ~ p => p.precondition = r; p }
  )

  def split: Parser[Split] = (
    ("[branched on " ~> variable <~ "]") ~ proof ~ proof ^^ { case id ~ l ~ r => Split(id, l, r) }
  )

  def proof: Parser[ProofStep] = split | step

  def parse(str: String): ProofStep = {
    val result = parseAll(proof, str)
    if (result.successful) {
      result.get
    } else {
      sys.error("could not parse:\n" + str)
    }
  }

}
