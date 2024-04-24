package util

// Base trait for all types
trait Type

// Parameter type used in function signatures
case class Param(name: String) extends Type

// Simple concrete types
case object BoolType extends Type
case object IntType extends Type
case object StringType extends Type

// A function type with input and output types
case class FunType(input: List[Type], output: Type) extends Type

// Represents a datatype with a name and parameter types
case class DataType(name: String, params: List[Type]) extends Type

// Example usage of complex types involving generics and parameters
case class ListType(elemType: Type) extends Type
case class MapType(keyType: Type, valueType: Type) extends Type

// Utility object for type operations
object TypeOps {
  // Checks if two types are equivalent
  def areEquivalent(t1: Type, t2: Type): Boolean = (t1, t2) match {
    case (BoolType, BoolType) => true
    case (IntType, IntType) => true
    case (StringType, StringType) => true
    case (ListType(et1), ListType(et2)) => areEquivalent(et1, et2)
    case (MapType(kt1, vt1), MapType(kt2, vt2)) => areEquivalent(kt1, kt2) && areEquivalent(vt1, vt2)
    case (FunType(in1, out1), FunType(in2, out2)) => 
      in1.zip(in2).forall { case (t1, t2) => areEquivalent(t1, t2) } && areEquivalent(out1, out2)
    case _ => false
  }

  // Example to demonstrate type checking
  def checkFunctionApplication(funType: FunType, argTypes: List[Type]): Boolean = {
    if (funType.input.length != argTypes.length) false
    else funType.input.zip(argTypes).forall { case (ft, at) => areEquivalent(ft, at) }
  }
}

