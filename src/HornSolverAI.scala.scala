//Horn Solver for Arrays and Lists with Abstract Interpretation
import ai.interpreter._

object HornSolverAI {

  def main(args: Array[String]): Unit = {
    val options = parseOptions(args.toList)
    val fileName = options.get("file").getOrElse("defaultFileName")

    //TODO boogie parse file -> create state/environment

    val env = ???

    options.getOrElse("mode", throw new IllegalArgumentException("Specify mode: abstract, symbolic, or horn")) match {
      case "abstract" =>
        val interpreter = Abstract
        ???

      // case "symbolic" =>
      //   val interpreter = Symbolic 
      //   ???

      case "horn" =>
        runHornSolver() 

      case _ =>
        throw new IllegalArgumentException("Invalid mode. Use abstract, symbolic, or horn.")
    }
  }

  def parseOptions(args: List[String]): Map[String, String] = {
    args.map { arg =>
      arg.split("=") match {
        case Array(key, value) => key.dropWhile(_ == '-') -> value
        case Array(key) => key.dropWhile(_ == '-') -> "true"
      }
    }.toMap
  }


  def runHornSolver(): Unit = {
    println("Horn Solver analysis not implemented yet...")
  }
}
