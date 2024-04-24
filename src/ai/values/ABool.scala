package ai.values

//ABool
sealed trait ABool extends AVal {
  def unary_! : ABool = this match {
      case AFalse   => ATrue
      case ATrue    => AFalse
      case AUnknown => AUnknown
  }

  def eq(that: AVal): ABool = (this, that) match {
      case (AFalse, AFalse) | (ATrue, ATrue) => ATrue
      case (AFalse, ATrue) | (ATrue, AFalse) => AFalse
      case (_: ABool, _: ABool)              => AUnknown
  }

  def noneq(that: AVal): ABool = (this, that) match {
      case (AFalse, AFalse) | (ATrue, ATrue) => AFalse
      case (AFalse, ATrue) | (ATrue, AFalse) => ATrue
      case (_: ABool, _: ABool)              => AUnknown
  }

  def &&(that: AVal): ABool = (this, that) match {
      case (ATrue, ATrue)                                       => ATrue
      case (AFalse, ATrue) | (ATrue, AFalse) | (AFalse, AFalse) => AFalse
      case (_: ABool, _: ABool)                                 => AUnknown
  }

  def ||(that: AVal): ABool = (this, that) match {
      case (ATrue, _) | (_, ATrue) => ATrue
      case (AFalse, AFalse)        => AFalse
      case (_: ABool, _: ABool)    => AUnknown
  }

  def meet(that: ABool): List[ABool] =(this, that) match {
    case (ATrue, ATrue) | (AUnknown, ATrue) | (ATrue, AUnknown) => List(ATrue)
    case (AFalse, AFalse) | (AUnknown, AFalse) | (AFalse, AUnknown) => List(AFalse)
    case (AUnknown, AUnknown) => List(AUnknown)
    case _ => List()
  }

  def join(that: ABool): ABool = {
    this widen that
  }
   
  def widen(that: ABool): ABool = {
    if(this == that) this
    else AUnknown
  }
}

case object ATrue extends ABool { def refs = List() }
case object AFalse extends ABool { def refs = List() }
case object AUnknown extends ABool { def refs = List() }