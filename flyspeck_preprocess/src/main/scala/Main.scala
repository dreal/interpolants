
import dzufferey.sexpr._
import dzufferey.arg.{Options, String => SString}
import java.io._

object Main extends Options {

  var in: Option[String] = None
  var out: Option[String] = None
  val usage = "..."

  newOption("-i", SString( s => in = Some(s) ), "input folder")
  newOption("-o", SString( s => out = Some(s) ), "output folder")

  def getFiles(i: String): Array[File] = {
    val folder = new File(i)
    assert(folder.isDirectory())
    folder.listFiles.filter(f => f.isFile && f.getName.endsWith(".smt2"))
  }

  def isConj(s: SExpr) = s match {
    case SApplication("assert", List(SApplication("not", List(SApplication("or", _))))) => true
    case _ => false
  }

  def getConj(s: SExpr): List[SExpr] = s match {
    case SApplication("assert", List(SApplication("not", List(lst)))) =>
      getConj(lst).map( s => SApplication("not", List(s)))
    case SApplication("or", lst) => lst.flatMap(getConj)
    case other => List(other)
  }

  def split(lst: List[SExpr]): (List[SExpr],List[SExpr],List[SExpr]) = {
    val idx = lst.indexWhere(isConj)
    assert(idx >= 0, "no conj")
    val prefix = lst.slice(0, idx)
    val conjs = getConj(lst(idx))
    val suffix = lst.slice(idx + 1, lst.length)
    (prefix, conjs, suffix)
  }

  def side(expr: SExpr, side: String): SExpr = {
    val s1 = SApplication("!", List(expr, SAtom(":side"), SAtom(side)))
    SApplication("assert", List(s1))
  }

  def mkPart(ses: List[SExpr], b: Int): Seq[(List[SExpr],List[SExpr])] = {
    //assert(ses.size >= b && b >= 0)
    assert(ses.size > b && b > 0)
    val s = ses.toArray
    def take(done: Int, pos: Int): Seq[(List[SExpr],List[SExpr])] = {
      if (done >= b) {
        Seq(Nil -> ses.drop(pos))
      } else if (b - done >= s.size - pos) {
        Seq(ses.drop(pos) -> Nil)
      }  else {
        val xs = take(done + 1, pos + 1).map{ case (a,b) => (s(pos) :: a, b) }
        val ys = take(done    , pos + 1).map{ case (a,b) => (a, s(pos) :: b) }
        xs ++ ys
      }
    }
    take(0, 0)
  }

  def allPartitions(lst: List[SExpr]): Seq[List[SExpr]] = {
    for (i <- 1 until lst.size;
         (a,b) <- mkPart(lst, i))
    yield (a.map(side(_, "A")) ::: b.map(side(_, "B")))
  }

  def name(f: File, c: Int): String = {
    val fn = f.getName
    val d = fn.indexOf('.')
    assert(d > 0)
    out.get + "/" + fn.substring(0, d) + "_" + c + ".smt2"
  }

  def save(f: File, c: Int, lst: List[SExpr]) {
    val outName = name(f, c)
    println(f.getName() + ": saving result to " + outName)
    val writer = new BufferedWriter(new FileWriter(outName))
    lst.foreach( se => {
      Printer(writer, se)
      writer.newLine
    })
    writer.close
  }

  def process(f: File) {
    val reader = new BufferedReader(new FileReader(f))
    Parser.parse(reader) match {
      case Some(lst) =>
        if (lst.exists(isConj)) {
          val (prefix, conjs, suffix) = split(lst)
          if (conjs.length > 1) {
            allPartitions(conjs).zipWithIndex.foreach{ case (c, i) => save(f, i, prefix ::: c ::: suffix) }
          } else {
            println(f.getName() + ": single clause (2)")
          }
        } else {
          println(f.getName() + ": single clause (1)")
        }
      case None =>
        println(f.getName() + ": parsing failed")
    }
  }

  def main(args: Array[String]) {
    apply(args)
    assert(in.isDefined, "no input directory")
    assert(out.isDefined, "no output directory")
    val files = getFiles(in.get)
    files.foreach(process)
  }

}
