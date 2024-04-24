package ai.values

//AInt
// Compare from: https://en.wikipedia.org/wiki/Interval_arithmetic
sealed trait IntInf extends Ordered[IntInf] {
  def unary_- : IntInf = this match {
    case NegInf     => PosInf
    case PosInf     => NegInf
    case Fin(value) => Fin(-value)
  }

  def +(that: Int): IntInf = this match {
    case NegInf | PosInf => this
    case Fin(value) => Fin(value + that)
  }

  def -(that: Int): IntInf = this match {
    case NegInf | PosInf => this
    case Fin(value) => Fin(value - that)
  }

  def +(that: IntInf): IntInf = (this, that) match {
    case (NegInf, PosInf) | (PosInf, NegInf) =>
      throw new IllegalArgumentException("Cannot add infinite values")
    case (_, NegInf) | (NegInf, _) => NegInf
    case (_, PosInf) | (PosInf, _) => PosInf
    case (Fin(a), Fin(b))          => Fin(a + b)
  }

  def -(that: IntInf): IntInf = (this, that) match {
    case (NegInf, NegInf) | (PosInf, PosInf) =>
      throw new IllegalArgumentException("Cannot subtract infinite values")
    case (_, NegInf) | (PosInf, _) => PosInf
    case (_, PosInf) | (NegInf, _) => NegInf
    case (Fin(a), Fin(b))          => Fin(a - b)
  }

  def *(that: IntInf): IntInf = (this, that) match {
    case (NegInf, NegInf) | (PosInf, PosInf)                   => PosInf
    case (NegInf, PosInf) | (PosInf, NegInf)                   => NegInf
    case (_, NegInf) | (NegInf, _) | (_, PosInf) | (PosInf, _) => this
    case (Fin(a), Fin(b))                                      => Fin(a * b)
  }

  def /(that: IntInf): IntInf = (this, that) match {
    case (NegInf, NegInf) | (PosInf, PosInf) => PosInf
    case (NegInf, PosInf) | (PosInf, NegInf) => NegInf
    case (_, NegInf) | (_, PosInf) =>
      throw new IllegalArgumentException("Cannot divide finite value by infinite value")
    case (Fin(a), Fin(b)) if b != 0 => Fin(a / b)
    case _ => throw new ArithmeticException("Division by zero")
  }

  def max(that: IntInf): IntInf = (this, that) match {
    case (NegInf, y: IntInf) => y
    case (x: IntInf, NegInf) => x
    case (PosInf, _) => PosInf
    case (_, PosInf) => PosInf
    case (Fin(a), Fin (b)) => Fin(a.max(b))
  }

  def min(that: IntInf): IntInf = (this, that) match {
    case (NegInf, y: IntInf) => NegInf
    case (x: IntInf, NegInf) => NegInf
    case (PosInf, y: IntInf) => y
    case (x: IntInf, PosInf) => x
    case (Fin(a), Fin (b)) => Fin(a.min(b))
  }

  // comparison operators <, <=, >, >= are then inherited from ordered

  /* Returns an integer whose sign communicates how x compares to y.
   *
   * The result sign has the following meaning:
   *
   *   - negative if x < y
   *   - positive if x > y
   *   - zero otherwise (if x == y)
   */
  def compare(that: IntInf): Int = (this, that) match {
    // WARNING: equality comparison only meaningful if it is interpreted as a bound
    case (NegInf, NegInf) => 0
    case (PosInf, PosInf) => 0
    case (NegInf, _)      => -1
    case (_, PosInf)      => -1
    case (PosInf, _)      => 1
    case (_, NegInf)      => 1
    case (Fin(a), Fin(b)) => a compare b
  }
}

object IntInf {
  implicit object ordering extends Ordering[IntInf] {
    def compare(x: IntInf, y: IntInf): Int = x compare y
  }
}
case object NegInf extends IntInf
case object PosInf extends IntInf
case class Fin(value: BigInt) extends IntInf

object AInt {
  val top: AInt = AInt(NegInf, PosInf)
  val zero: AInt = AInt(0)
  val one: AInt = AInt(1)
  def apply(n: BigInt): AInt = AInt(Fin(n), Fin(n))
}

case class AInt(lb: IntInf, ub: IntInf) extends AVal {
  require (lb <= ub)
  def refs = List()

  def widen(that: AInt): AInt = {
    val lb = if (that.lb < this.lb) NegInf else this.lb
    val ub = if (that.ub > this.ub) PosInf else this.ub
    AInt(lb, ub)
  }

  def narrow(that: AInt): AInt = {
    val lb = if (this.lb == NegInf) that.lb else this.lb
    val ub = if (this.ub == PosInf) that.ub else this.ub
    AInt(lb, ub)
  }

  //returns [x,y] if x≤y otherwise requirement failed
  def meet(that: AInt): List[AInt] = {
    val lb = this.lb.max(that.lb)
    val ub = this.ub.min(that.ub)
    if (lb <= ub) List(AInt(lb, ub)) else List()
  }

  def join(that: AInt): AInt = {
    val lb = this.lb.min(that.lb)
    val ub = this.ub.max(that.ub)
    AInt(lb, ub)
  }

  override def toString: String = (lb, ub) match {
    case (NegInf, PosInf)   => "(-∞,∞)"
    case (Fin(lb), PosInf)  => "[" + lb + ",∞)"
    case (NegInf, Fin(ub))  => "(-∞," + ub + "]"
    case (Fin(lb), Fin(ub)) => "[" + lb + "," + ub + "]"
    case _                  => throw new IllegalArgumentException("Invalid AInt bounds") // does not satisfy lb <= ub
  }

  def unary_- : AInt = {
    AInt(-ub, -lb)
  }

  def +(that: AInt): AInt = {
    AInt(this.lb + that.lb, this.ub + that.ub)
  }

  def -(that: AInt): AInt = {
    AInt(this.lb - that.lb, this.ub - that.ub)
  }

  def *(that: AInt): AInt = {
    import IntInf.ordering
    val vs = List(this.lb * that.lb,this.lb * that.ub,this.ub * that.lb,this.ub * that.ub)
    AInt(vs.min, vs.max)
  }

  def /(that: AInt): AInt = {
    import IntInf.ordering
    val vs = List(this.lb / that.lb,this.lb / that.ub,this.ub / that.lb,this.ub / that.ub)
    AInt(vs.min, vs.max)
  }

  def <(that: AInt): ABool = {
    if(this.ub < that.lb) ATrue 
    else if(that.ub <= this.lb) AFalse
    else AUnknown
  }

  def <=(that: AInt): ABool = {
    if(this.ub <= that.lb) ATrue 
    else if(that.ub < this.lb) AFalse
    else AUnknown
  }

  def >(that: AInt): ABool = {
    that < this
  }

  def >=(that: AInt): ABool = {
    that <= this
  }

  def ==(that: AInt): ABool = 
    (this, that) match {
      case (AInt(NegInf, _), _) | (AInt(_, PosInf), _)=> AUnknown
      case (_, AInt(NegInf, _)) |  (_, AInt(_, PosInf)) => AUnknown
      case (AInt(Fin(lb1), Fin(ub1)), AInt(Fin(lb2), Fin(ub2))) => //can only compare AInt with Fin-Boundaries
        if(lb1 == ub1 && lb2 == ub2 && lb1 == lb2) ATrue  //really the same boundaries
        else if (( (lb1 < lb2) && (lb2 <= ub1) ) || (lb2 < lb1) && (lb1 <= ub2)) AUnknown //intervals are overlaping  
        else if((lb1 < lb2 && ub2 < ub1) || (lb2 < lb1 && ub1 < ub2)) AUnknown //one interval overlaps/contains the other 
        else AFalse //just not equal
  }

  def !=(that: AInt): ABool = !(this == that)
}