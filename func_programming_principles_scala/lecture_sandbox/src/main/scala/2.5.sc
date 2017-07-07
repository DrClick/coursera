import scala.annotation.tailrec

//Companion Object of Rational
object Rational{
  //Provides a factory method for the alternate constructor which
  //only requires the enumerator and assumes 1 for the denominator
  def apply(x: Int): Rational = new Rational(x)
}


case class Rational(_numer: Int, _denom: Int) {

  require(_denom != 0, "Denominator must not be zero")
  def this(x: Int) = this(x, 1)


  private val _gcd = gcd(_numer, _denom)
  val numer: Int = _numer / _gcd
  val denom: Int = _denom / _gcd

  //GCD is defined recursively as taking the modulo operator until the
  @tailrec
  private def gcd(a: Int, b: Int): Int = {
    if (b == 0) math.abs(a) else gcd(b, a % b)
  }

  private def operation(f: (Int, Int) => Int)(that: Rational): Rational = {
    val _numer = f(numer * that.denom, that.numer * denom)
    val _denom = denom * that.denom
    Rational(_numer, _denom)
  }

  def unary_- : Rational = Rational(-numer, denom)

  def <(that: Rational): Boolean =
    this.numer * that.denom < this.denom * that.numer

  def >(that: Rational): Boolean =
    this.numer * that.denom > this.denom * that.numer

  def compare(that: Rational): Boolean =
    this.numer * that.denom == this.denom * that.numer

  def max(that: Rational): Rational =
    if (this < that) that else this

  def +(that: Rational, lowest_form: Boolean = true): Rational =
    operation((a, b) => a + b)(that)

  def -(that: Rational): Rational =
    operation((a, b) => a - b)(that)

  def *(that: Rational): Rational =
    Rational(this.numer * that.numer, this.denom * that.denom)

  override def toString: String = numer + "/" + denom
}

val a = Rational(3, 5)
val b = Rational(6, 4)
val c = Rational(5)
val d = Rational(1, 2)

a - b
a + b
-a
a max b
c * d










