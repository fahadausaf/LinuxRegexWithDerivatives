import scala.language.implicitConversions
import scala.language.reflectiveCalls


object ExtendedDerivatives {
  abstract class Rexp
  case object ZERO extends Rexp
  case object ONE extends Rexp
  case class CHAR(c: Char) extends Rexp
  case class ALT(r1: Rexp, r2: Rexp) extends Rexp
  case class SEQ(r1: Rexp, r2: Rexp) extends Rexp
  case class STAR(r: Rexp) extends Rexp
  case class RECD(x: String, r: Rexp) extends Rexp
  case class QUESTION(r: Rexp) extends Rexp
  case class PLUS(r: Rexp) extends Rexp
  case object ANY extends Rexp

  implicit def string2rexp(s: String): Rexp = charlist2rexp(s.toList)

  implicit def RexpOps(r: Rexp) = new {
    def |(s: Rexp) = ALT(r, s)
    def % = STAR(r)
    def ~(s: Rexp) = SEQ(r, s)
    def ? = QUESTION(r)
  }

  implicit def stringOps(s: String) = new {
    def |(r: Rexp) = ALT(s, r)
    def |(r: String) = ALT(s, r)
    def % = STAR(s)
    def ~(r: Rexp) = SEQ(s, r)
    def ~(r: String) = SEQ(s, r)
    def $(r: Rexp) = RECD(s, r)
    def ?(r: Rexp) = ALT(ONE, r)
    def ?(r: String) = ALT(ONE, r)
  }

  // some convenience for typing in regular expressions
  def charlist2rexp(s: List[Char]): Rexp = s match {
    case Nil => ONE
    case c :: Nil => CHAR(c)
    case c :: s => SEQ(CHAR(c), charlist2rexp(s))
  }

  val ASCI: Rexp =
    "a"|"b"|"c"|"d"|"e"|"f"|"g"|"h"|"i"|"j"|"k"|"l"|"m"|"n"|"o"|"p"|"q"|"r"|"s"|"t"|"u"|"v"|"w"|"x"|"y"|"z"|
    "A"|"B"|"C"|"D"|"E"|"F"|"G"|"H"|"I"|"J"|"K"|"L"|"M"|"N"|"O"|"P"|"Q"|"R"|"S"|"T"|"U"|"V"|"W"|"X"|"Y"|"Z"|
    "0"|"1"|"2"|"3"|"4"|"5"|"6"|"7"|"8"|"9"
  val any: Rexp = STAR(ALT(ONE,ASCI))

  def nullable(r: Rexp): Boolean = r match {
    case ZERO         => false
    case ONE          => true
    case CHAR(_)      => false
    case ALT(r1, r2)  => nullable(r1) || nullable(r2)
    case SEQ(r1, r2)  => nullable(r1) && nullable(r2)
    case STAR(_)      => true
    case RECD(_, r1)  => nullable(r1)
  }

  def der(c: Char, r: Rexp): Rexp = r match {
    case ZERO         => ZERO
    case ONE          => ZERO
    case CHAR(d)      => if (c == d) ONE else ZERO
    case ALT(r1, r2)  => ALT(der(c, r1), der(c, r2))
    case SEQ(r1, r2)  =>
      if (nullable(r1)) ALT(SEQ(der(c, r1), r2), der(c, r2))
      else SEQ(der(c, r1), r2)
    case STAR(r)      => SEQ(der(c, r), STAR(r))
    case RECD(_, r1)  => der(c, r1)
    case QUESTION(r)  => der(c, ALT(ONE, r))
    case PLUS(r)      => der(c, SEQ(r, STAR(r)))
    case ANY          => der(c, any)
  }

  // derivative w.r.t. a string (iterates der)
  def ders(s: List[Char], r: Rexp): Rexp = s match {
    case Nil => r
    case c :: s => ders(s, der(c, r))
  }

  def lex(r: Rexp, s: List[Char]): Boolean = s match {
    case Nil => if (nullable(r)) true else false
    case c :: cs => lex(der(c, r), cs)
  }

  def lexing(r: Rexp, s: String): Boolean = lex(r, s.toList)

  def main(args: Array[String]): Unit = {
    println("Derivatives")

    val K: Rexp = "a" | "b"
    val I: Rexp = "ab" | "ba"

    //println(lexing((K | I).%, "abab"))

    val r1: Rexp = "a" | "ab"
    val r2: Rexp = "b" | ONE
    val r3: Rexp = r1 ~ r2
    val r: Rexp = QUESTION(r3)
    //println(lexing(r, "ab"))

    val a: Rexp = ANY
    println(lexing(a, "fahad"))

  }


}

