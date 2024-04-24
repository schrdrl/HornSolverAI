package util

// Base trait for all expression types
trait Expr 

// Variables
case class Var(name: String, typ: Type) extends Expr

// Literals
case class Lit(value: Any) extends Expr

// Equality expression
case class Eq(lhs: Expr, rhs: Expr) extends Expr

// Logical operations
case class Not(expr: Expr) extends Expr
case class And(lhs: Expr, rhs: Expr) extends Expr
case class Or(lhs: Expr, rhs: Expr) extends Expr
case class Imp(lhs: Expr, rhs: Expr) extends Expr

// Arithmetic operations
case class UMinus(expr: Expr) extends Expr
case class Plus(lhs: Expr, rhs: Expr) extends Expr
case class Minus(lhs: Expr, rhs: Expr) extends Expr
case class Times(lhs: Expr, rhs: Expr) extends Expr
case class DivBy(lhs: Expr, rhs: Expr) extends Expr
case class Mod(lhs: Expr, rhs: Expr) extends Expr

// Comparison operators
case class Lt(lhs: Expr, rhs: Expr) extends Expr
case class Le(lhs: Expr, rhs: Expr) extends Expr
case class Gt(lhs: Expr, rhs: Expr) extends Expr
case class Ge(lhs: Expr, rhs: Expr) extends Expr

// The true and false expressions
case object True extends Expr
case object False extends Expr
