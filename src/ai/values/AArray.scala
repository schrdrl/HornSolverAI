package ai.values

import util._

/* 
AArray - Represents an immutable abstract array with bounds and a default value

Assumptions:
 - [-1,-1] => empty Array with value 
 - [0,n] => non-empty array with n fields, index starts with 0 and goes to n-1 

 */ 
case class AArray(borders: AInt, value: AVal) extends AVal {
  require (Fin(0) <= borders.lb, "Borders must be non-negative") //no negative bounds
  def refs = value.refs ++ borders.refs

  override def toString: String = {
    (borders.lb, borders.ub) match {
      case (Fin(l), Fin(u)) if l == u && l == 0 => "[]"
      case (Fin(l), Fin(u)) =>
        val values = List.fill((u - l).toInt)(value)
        "[" + values.map(v => v.toString).mkString(",") + "]"
      case _ => s"[Undefined array bounds: ${borders.lb} to ${borders.ub}]"
    }
  }

  private def isTrue(condition: ABool): Boolean = condition match {
    case ATrue => true
    case _ => false
  }

  // Select method checks if the index is within the bounds and returns the corresponding value or undefined
  def select(index: AInt): AVal = {
    require(isTrue(AInt(0) <= index), "Index must be non-negative")
    if (index.lb >= borders.lb && index.ub < borders.ub) {
      value
    } else {
      println(s"Index $index out of bounds. No select performed.")
      AUnknown  // Outside of bounds, return undefined 
    }
  }

  def store(index: AInt, newValue: AVal): AArray = {
    require(isTrue(AInt(0) <= index), "Index must be non-negative")
    if (index.lb >= borders.lb && index.ub <= borders.ub) {
      // Creating a new array value where only the specified index is updated
      (value, newValue) match
        case (value : AInt, newValue : AInt) => 
          val updatedValue = value `join` newValue
          AArray(borders, updatedValue)
    } else {
      println(s"Index $index out of bounds. No store performed.")
      this  // If the index is outside the bounds, return the array unchanged
    }
  }
}

object AArray {
  // Define an empty AArray with bounds beeing -1 and an undefined value
  def empty: AArray = AArray(AInt(-1), AUnknown)
}

object AArrayTest extends App {
  def example(): Unit = {
    val emptyArray = AArray.empty
    println("Created empty array: " + emptyArray)
    println("Attempt to select from empty array: " + emptyArray.select(AInt(0)))

    val normalArray = AArray(AInt(Fin(0), Fin(10)), AInt(Fin(1), Fin(1)))
    println("Created non-empty array: " + normalArray)
    println("Select from normal array at index 5: " + normalArray.select(AInt(5)))
    val updateArray = normalArray.store(AInt(5), AInt(2))
    println("Update array at index 5: " + updateArray.select(AInt(5)))
    println("Updated array: " + updateArray)
    val outOfBoundsUpdate = normalArray.store(AInt(11), AInt(3))
    println("Attempt to update normal array at index 11 (out of bounds): " + outOfBoundsUpdate.select(AInt(11)))
  }

  example()
}