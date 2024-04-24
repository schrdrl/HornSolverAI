package ai.values
import util._

//References 
class ARef(name: String, val index: Int) {
  override def toString(): String = {
    this.name.toString() + index
  }
}

object ARef extends Counter {
  def fresh(name: String) = {
    new ARef(name, next)
  }

  def fresh() = {
    new ARef("ref", next)
  }
}

//Values
trait AVal {
  def refs: List[ARef]
}




