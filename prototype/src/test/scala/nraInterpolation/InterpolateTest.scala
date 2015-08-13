package nraInterpolation

import org.scalatest._
import nraInterpolation.Common._
import nraInterpolation.Utils._

class InterpolateTest extends FunSuite {

  val files = Seq(
    "test01"
  )

  def testInterpolant(f: String) {

    Interpolate.delta = 0.01
    Interpolate.deltaCheck = 0.001
    Interpolate.w = 0.05

    val fa = f + "a.txt"
    val fb = f + "b.txt"
    val fd = f + "d.txt"

    val a = parseFormulaFromFile(resources + fa)
    val b = parseFormulaFromFile(resources + fb)
    val d = parseBoundsFromFile(resources + fd)

    val itp = new Interpolate(a, b, d)
    assert(itp.check)
  }

  test("001"){ testInterpolant(files(0)) }

}
