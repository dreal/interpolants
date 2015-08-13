package nraInterpolation

import dzufferey.utils._
import java.io._

object Common {

  val resources = "src/test/resources/"

  def readFile(f: String) = IO.readTextFile(resources + f)
    
  def openFile(f: String) = new BufferedReader(new FileReader(resources + f))

}
