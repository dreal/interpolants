package nraInterpolation.onTheFly

import org.scalatest._
import nraInterpolation.Common._

class ProofParserTest extends FunSuite {

  val file = Seq("newProofFormat01.txt",
                 "newProofFormat02.txt",
                 "newProofFormat03.txt",
                 "newProofFormat04.txt")
  
  val domains =   Seq( 1,   1,     1,  1)
  val splits =    Seq( 5,  15,  1023,  1)
  val contracts = Seq(82, 731, 13883, 66)
  val conflicts = Seq( 6,  16,  1024,  2)

  def isSplit(e: ProofEvent)    = e match { case Split(_,_,_) => true;      case _ => false }
  def isDomain(e: ProofEvent)   = e match { case Domain(_) => true;         case _ => false }
  def isContract(e: ProofEvent) = e match { case Contraction(_,_) => true;  case _ => false }
  def isConflict(e: ProofEvent) = e match { case Conflict(_) => true;       case _ => false }

  def count(idx: Int) {
    val content = readFile(file(idx))
    val prf = Parser.parseAllSteps(content)
    assert(prf.filter(isDomain).size   == domains(idx))
    assert(prf.filter(isSplit).size    == splits(idx))
    assert(prf.filter(isContract).size == contracts(idx))
    assert(prf.filter(isConflict).size == conflicts(idx))
  }

  def countEvt(idx: Int) {
    val reader = openFile(file(idx))
    var d = 0
    var s = 0
    var ct = 0
    var cf = 0
    def fct(e: ProofEvent) = e match {
      case Domain(_) =>         d  += 1
      case Split(_,_,_) =>      s  += 1
      case Contraction(_,_) =>  ct += 1
      case Conflict(_) =>       cf += 1
    }
    Parser.parseAllSteps(fct, reader)
    reader.close
    assert(d  == domains(idx))
    assert(s  == splits(idx))
    assert(ct == contracts(idx))
    assert(cf == conflicts(idx))
  }

  test("001") { count(0) } 
  test("002") { countEvt(0) } 
  test("003") { count(1) } 
  test("004") { countEvt(1) } 
  test("005") { count(2) } 
  test("006") { countEvt(2) } 
  test("007") { count(3) } 
  test("008") { countEvt(3) } 
  
}
