package util

// Base trait for all statement types
trait Stmt

// Block of statements
case class Block(progs: List[Stmt]) extends Stmt 

// Local variable declarations
case class Local(vars: List[Var], exprs: List[Expr]) extends Stmt

// Assignment statement
case class Assign(vars: List[Var], exprs: List[Expr]) extends Stmt

case class Assert(cond: Expr) extends Stmt
case class Assume(cond: Expr) extends Stmt

// Conditional statement
case class If(cond: Expr, thenStmt: Stmt, elseStmt: Stmt) extends Stmt

// Loop construct with an optional invariant
case class While(cond: Expr, body: Stmt, max: Int = Int.MaxValue) extends Stmt
