
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
    if (folder.isDirectory) {
      folder.listFiles.filter(f => f.isFile && f.getName.endsWith(".smt2"))
    } else {
      assert(folder.getName.endsWith(".smt2"))
      Array(folder)
    }
  }

  def nnf(s: SExpr, neg: Boolean = false): SExpr = s match {
    case SApplication("not", List(a)) => nnf(a, !neg)
    case SApplication("and", lst) =>
      val s = if (neg) "or" else "and"
      SApplication(s, lst.map(nnf(_, neg)))
    case SApplication("or", lst) =>
      val s = if (neg) "and" else "or"
      SApplication(s, lst.map(nnf(_, neg)))
    case s @ SApplication("<="|"<", lst) =>
      val s = if (neg) ">=" else "<="
      SApplication(s, lst)
    case s @ SApplication(">="|">", lst) =>
      val s = if (neg) "<=" else ">="
      SApplication(s, lst)
    case other => if (neg) SApplication("not", List(other)) else other
  }

  def flatten(s: SExpr): List[SExpr] = s match {
    case SApplication("and", lst) => lst.flatMap(flatten)
    case other => List(other)
  }

  def mapFormula(fct: SExpr => SExpr, lst: List[SExpr]): List[SExpr] = lst.map{
    case SApplication("assert", List(f)) => SApplication("assert", List(fct(f)))
    case other => other
  }
  
  def flatMapFormula(fct: SExpr => List[SExpr], lst: List[SExpr]): List[SExpr] = lst.flatMap{
    case SApplication("assert", List(f)) => fct(f).map(f => SApplication("assert", List(f)))
    case other => List(other)
  }

  def isBound(s: SExpr): Boolean = s match {
    case SApplication("assert", List(f)) => isBound(f)
    case s @ SApplication("<="|"<"|">="|">", List(SAtom(_),SAtom(_))) => true
    case s @ SApplication("<="|"<"|">="|">", List(SAtom(_),SAtom(_),SAtom(_))) => true
    case other => false
  }

  def isAssert(s: SExpr): Boolean = s match {
    case SApplication("assert", List(f)) => true
    case other => false
  }

  def getConj(s: SExpr): List[SExpr] = s match {
    case SApplication("assert", List(f)) => getConj(f)
    case SApplication("and", lst) => lst.flatMap(getConj)
    case other => List(other)
  }

  def split(lst: List[SExpr]): (List[SExpr],List[SExpr],List[SExpr]) = {
    val idx1 = lst.indexWhere(isAssert)
    val idx2 = lst.lastIndexWhere(isAssert)
    val pre1 = lst.slice(0, idx1)
    val suffix = lst.slice(idx2+1, lst.length)
    val (b,c) = lst.slice(idx1,idx2+1).partition(isBound)
    assert(c.forall(isAssert))
    (pre1 ::: b, c, suffix)
  }
  
  def side(expr: SExpr, s: String): SExpr = expr match {
    case SApplication("assert", lst) => SApplication("assert", lst.map(side(_, s)))
    case SApplication("and", lst) => SApplication("and", lst.map(side(_, s)))
    case SApplication("or",  lst) => SApplication("or",  lst.map(side(_, s)))
    case expr => SApplication("!", List(expr, SAtom(":side"), SAtom(s)))
  }

  def mkPart(ses: List[SExpr], b: Int): Seq[(List[SExpr],List[SExpr])] = {
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
    if (lst.length > 6) {
      println("too large for all partition")
      for (i <- 1 until lst.size-1) yield (lst.take(i).map(side(_, "A")) ::: lst.drop(i).map(side(_, "B")))
    } else {
      for (i <- 1 until lst.size;
           (a,b) <- mkPart(lst, i))
      yield (a.map(side(_, "A")) ::: b.map(side(_, "B")))
    }
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
      case Some(l0) =>
        val l1 = mapFormula(nnf(_), l0)
        val l2 = flatMapFormula(flatten, l1)
        val (prefix,conjs,suffix) = split(l2)
        if (conjs.length > 1) {
          allPartitions(conjs).zipWithIndex.foreach{ case (c, i) => save(f, i, prefix ::: c ::: suffix) }
        } else {
          println(f.getName() + ": single clause")
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
