package nraInterpolation

import org.scalatest._
import nraInterpolation.Common._

class ProofParserTest extends FunSuite {

  test("001"){
    val content = readFile("proof01.txt")
    val parsed = ProofParser.parse(content) 
    //Console.println(parsed.toString)
    ()
  }
  
  test("002"){
    val content = readFile("proof02.txt")
    val parsed = ProofParser.parse(content) 
    //Console.println(parsed.toString)
    //Console.println(parsed.head.asInstanceOf[Contraction].pruned)
    ()
  }
  
  test("003"){
    val content = readFile("proof03.txt")
    val parsed = ProofParser.parse(content) 
    //Console.println(parsed.toString)
    ()
  }

}
