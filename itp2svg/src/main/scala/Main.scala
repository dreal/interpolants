
import dzufferey.sexpr._
import dzufferey.arg.{Options, String => SString}
import dzufferey.smtlib._
import java.io._

object Main extends Options {

  var in: Option[String] = None
  var out: Option[String] = None
  var x = "x"
  var y = "y"
  val usage = "..."

  newOption("-i", SString( s => in = Some(s) ), "input file")
  newOption("-o", SString( s => out = Some(s) ), "output file")
  newOption("-x", SString( s => x = s ), "x axis")
  newOption("-y", SString( s => y = s ), "y axis")

  def sexpr2formula(s: SExpr): Formula = s match {
    case SAtom("true") => True()
    case SAtom("false") => False()
    case SAtom(a) =>
      try {
        Literal(a.toDouble)
      } catch {
        case _: Throwable =>
          Variable(a)
      }
    case SApplication(sym, lst) if InterpretedFct knows sym =>
      InterpretedFct(sym).get.apply(lst.map(sexpr2formula):_*)
    case other => sys.error("unexpected: " + other)
  }

  def printCubes(cubes: Iterable[Map[Variable,(Double,Double)]]) {
    println("cubes ("+cubes.size+"):")
    for (m <- cubes) {
      val s = m.map{ case (v, (l,u)) => v + "∈["+l+","+u+"]" }.mkString("  ", "\t","")
      println(s)
    }
  }

  def toSVG(fname: String, cubes: Iterable[Map[Variable,(Double,Double)]]) {
    val vx = Variable(x)
    val vy = Variable(y)
    println("saving to " + fname)
    val writer = new BufferedWriter(new FileWriter(fname))
    writer.write("<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\">")
    writer.newLine
    for (c <- cubes) {
      val (lx,ux) = c(vx)
      val (ly,uy) = c(vy)
      writer.write("<rect x=\""+lx+"\" y=\""+ly+"\" height=\""+(uy-ly)+"\" width=\""+(ux-lx)+"\" style=\"fill: #aa0000\"/>")
      writer.newLine
    }
    writer.write("</svg>")
    writer.newLine
    writer.close
  }

  def main(args: Array[String]) {
    apply(args)
    assert(in.isDefined, "no input file")
    val reader = new BufferedReader(new FileReader(in.get))
    dzufferey.sexpr.Parser.parse(reader) match {
      case Some(sitp :: sbounds) =>
        val itp = sexpr2formula(sitp)
        println("interpolant " + itp)
        val bounds = sbounds.foldLeft(Map[Variable,(Double,Double)]())( (acc, s) => {
          sexpr2formula(s) match {
            case LowerBound(v, d) =>
              val (l, u) = acc.getOrElse(v, (Double.NegativeInfinity, Double.PositiveInfinity))
              acc + (v -> (math.max(l,d), u))
            case UpperBound(v, d) =>
              val (l, u) = acc.getOrElse(v, (Double.NegativeInfinity, Double.PositiveInfinity))
              acc + (v -> (l,math.min(u,d)))
            case other =>
              sys.error("unexpected: " + other)
          }
        })
        println("bounds " + bounds)
        val cubes = hyperCubes(bounds, itp).toSet
        printCubes(cubes)
        val toProject = itp.freeVariables.filter( v => v.name != x && v.name != y )
        println("projecting out: " + toProject ) 
        val cubes2 = cubes.map( _ -- toProject )
        printCubes(cubes2)
        out.foreach( o => toSVG(o, cubes2) )
      case other =>
        sys.error("could not parse " + in.get + ": " + other)
    }
  }
  
  def hyperCubes(interpolant: Formula): Set[Map[Variable,(Double,Double)]] = {
    hyperCubes(Double.NegativeInfinity, Double.PositiveInfinity, interpolant)
  }
  
  def hyperCubes(lb: Double, ub: Double, interpolant: Formula): Set[Map[Variable,(Double,Double)]] = {
    val init = interpolant.freeVariables.map(v => v -> (lb, ub)).toMap
    hyperCubes(init, interpolant)
  }
  
  def hyperCubes(commonVariablesDomain: Map[Variable,(Double,Double)],
                 interpolant: Formula): Set[Map[Variable,(Double,Double)]] = {
    def traverse(f: Formula, acc: Map[Variable,(Double,Double)]): Set[Map[Variable,(Double,Double)]] = f match {
      case And(args @ _*) =>
        args.foldLeft(Set(acc))( (acc , a) => acc.flatMap( ac => traverse(a, ac)) )
      case Or(args @ _*) =>
        args.flatMap( a => traverse(a, acc) ).toSet
      case UpperBound(v, d) =>
        val (l, u) = acc(v)
        val m = acc + (v -> (l, math.min(d, u)))
        Set(m)
      case LowerBound(v, d) =>
        val (l, u) = acc(v)
        val m = acc + (v -> (math.max(d, l), u))
        Set(m)
      case other =>
        sys.error("unexpected: " + other)
    }
    val cubes = traverse(interpolant, commonVariablesDomain)
    //filter the empty ones
    cubes.filter( c => c.forall{ case (_, (l, u)) => l <= u} )
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
