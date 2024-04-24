// package ai.interpreter

// import util._

// case class SState(path: List[Expr], map: Map[Var, Expr]) {
//     def and(that: Expr) =
//         copy(path = that :: path)
// }

// object SState {
//     val empty = SState(Nil, Map()) 
// }

// abstract class Symbolic extends Interpreter[SState] {
//     def assign(xs: List[Var], rhs: List[Expr], states: List[SState]): List[SState] = {
//         xs.zip(rhs).foldLeft(states) { (currentStates, pair) =>
//             val (a, expr) = pair
//             currentStates.map { state =>
//                 state.copy(map = state.map + (a -> expr))
//             }
//         }
//     }

//     def positive(expr: Expr, states: List[SState]): List[SState] = expr match {
//         case True =>
//             states

//         case False =>
//             List()

//         case And(phis) =>
//             phis.foldLeft(states) { (acc, phi) => positive(phi, acc)}

//         case Or(phis) =>
//             phis.foldLeft(List[SState]()) { (acc, phi) => positive(phi, states) ++ negative(phi, acc) }

//         case Imp(phi, psi) =>  // with (A => B) <=> (¬A ∨ B)
//             val notPhi = Not(phi) // ¬A
//             val orResult = Or(List(notPhi, psi)) // ¬A ∨ B
//             List(orResult).foldLeft(List[SState]()) { (acc, phi) => positive(phi, states) ++ negative(phi, acc) }

//         case Not(phi) =>
//             negative(phi, states)

//         case _ =>
//             states map (_ and expr)

//     }

//     def negative(expr: Expr, states: List[SState]): List[SState] = expr match {
//         case True => List()

//         case False => states
        
//         case And(phis) => 
//             phis.foldLeft(List[SState]()) { (acc, phi) => negative(phi, states) ++ positive(phi, acc)}
     
//         case Or(phis) => 
//             phis.foldLeft(states) { (acc, phi) => negative(phi, acc)}
        
//         case Imp(phi, psi) => 
//             val notPhi = Not(phi) // ¬A
//             val orResult = Or(List(notPhi, psi)) // ¬A ∨ B
//             List(orResult).foldLeft(states) { (acc, phi) => negative(phi, acc)}
        
//         case Not(phi) => positive(phi, states)
        
//         case _ =>
//             states map (_ and !expr)
//     }

//     //TODO recheck widen for Symbolic Interpreter
//     //Simplification of the path condition and combination of states
//     def widen(st1: SState, st2: SState): SState /* = {
//         SState(Or(st1.path, st2.path), st1.map ++ st2.map.filterKeys(!st1.map.contains(_))) //TODO
//     } */

//     def widen(states: List[SState]): List[SState] = {
//         if (states.isEmpty) List() else List(states reduceLeft widen)
//     }

//     //Refinement of the path condition and combination of states
//     def narrow(st1: SState, st2: SState): SState /* = {
//         SState(And(st1.path, st2.path), st1.map ++ st2.map.filterKeys(!st1.map.contains(_))) //TODO
//     } */

//     def narrow(states: List[SState]): List[SState] = {
//         if (states.isEmpty) List() else List(states reduceLeft narrow)
//     }

//     def isSubsetOf(states1: List[SState], states2: List[SState]): Boolean = {
//         states1.forall(s1 => states2.exists(s2 => s1.path == s2.path && s1.map == s2.map)) //TODO
//         }

//     def invariants(states: List[SState]): Expr = {
//         val invariants = for (state <- states) yield {
//             val formulas = for ((name, expr) <- state.map) yield {
//                 //Idea: A simple invariant could be that the expression for a variable is equal to the symbolic expression
//                 Eq(name, expr)
//             }
//             And(state.path ++ formulas.toList)
//         } 
//         Or(invariants.toList)
//         }

//     def local(xs: List[Var], rhs: List[Expr], states: List[SState]): List[SState] = {
//         states.map(state => state.copy(map = state.map ++ (xs zip rhs)))
//     }
// }   


// object Symbolic extends Stage {

//   def exec(prefix: List[Cmd], cmds: List[Cmd], last: Cmd, state: cuvee.State): List[Cmd] = {
//     cmds.flatMap {
//       case DefineProc(name, params, in, out, spec, body) =>
//         val proc = Proc(name, params, in, out, spec)
//         val s: Symbolic = ??? // new Symbolic()
//         var st_s: SState = SState.empty

//         for (x <- in ++ out) {
//             // Initialize each variable as a symbolic value
//             st_s = st_s.copy(map = st_s.map + (x -> Var(x.name, x.typ)))
//         }

//         println("Program variable(s):")
//         println(st_s)

//         var s_s: List[SState] = List(st_s)
//         val res_s = s.exec(body, s_s)
//         println("\nSymbolic: exec " + name + " = ")
//         for (r <- res_s) println(r)
//         println()

//         List(DefineProc(name, params, in, out, spec, body))

//       case cmd =>
//         List(cmd)
//     }
//   }
// }
