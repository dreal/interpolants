package nraInterpolation

import org.scalatest._
import dzufferey.utils._

class ProofParserTest extends FunSuite {

  val resources = "src/test/resources/"

  test("001"){
    val content = IO.readTextFile(resources + "proof01.txt")
    val parsed = ProofParser.parse(content) 
    //Console.println(parsed.toString)
    ()
  }
  
  test("002"){
    val content = IO.readTextFile(resources + "proof02.txt")
    val parsed = ProofParser.parse(content) 
    //Console.println(parsed.toString)
    //Console.println(parsed.head.asInstanceOf[Contraction].pruned)
    ()
  }
  
  test("003"){
    val content = IO.readTextFile(resources + "proof03.txt")
    val parsed = ProofParser.parse(content) 
    //Console.println(parsed.toString)
    ()
  }
}
