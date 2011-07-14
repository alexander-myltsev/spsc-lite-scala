package flowanalyser

import spsc._
import spsc.multi._
import scala.collection.immutable

object SampleFlowM {

  val printResults = true

  val program1 = """
gEven(Z()) = True();
gEven(S(x)) = gOdd(x);
gOdd(Z()) = False();
gOdd(S(x)) = gEven(x);
gDouble(Z()) = Z();
gDouble(S(x)) = S(S(gDouble(x)));
gAdd(S(x), y) = gAdd(x, S(y));
gAdd(Z(), y) = y;
"""
  val target1 = "gOdd(gAdd(x, S(S(Z()))))"
  val varTreeGrammar1 = """
x -> Z();
x -> S(x);
"""

  val varTreeGrammar4 = """
a -> Z();
a -> S(a);
b -> Z();
b -> S(b);
"""

  def main(args: Array[String]): Unit = {
    runFlowAnalyserM(target1, program1, varTreeGrammar1, HEWithRedexWhistle)

    return

    runFlowAnalyserM(spsc.Sample.target4, spsc.Sample.program4, varTreeGrammar4, SimpleWhistle)
    runFlowAnalyserM(spsc.Sample.target4, spsc.Sample.program4, varTreeGrammar4, HEWhistle)
    runFlowAnalyserM(spsc.Sample.target4, spsc.Sample.program4, varTreeGrammar4, HEWithRedexWhistle)
    runFlowAnalyserM(spsc.Sample.target4, spsc.Sample.program4, varTreeGrammar4, HEByCouplingWhistle)
    runFlowAnalyserM(spsc.Sample.target4, spsc.Sample.program4, varTreeGrammar4, HEByCouplingWithRedexWhistle)

    println("<<<<<<<<<<<")
  }

  def runFlowAnalyserM(targetText: String, programText: String, varTreeGrammarText: String, whistle: Whistle) = {
    val program = SParsers.parseProg(programText)
    val target = SParsers.parseTerm(targetText)
    val sc = new MultiResultSuperCompiler(whistle, program)

    val varTreeGrammar = SParsers.parseTreeGrammar(varTreeGrammarText)

    println("** runFlowAnalyserM with " + whistle.name + " **")
    println(target)
    println()
    println(program)

    val pts = sc.buildTrees(target)

    val ress = pts map { (x => (new ResidualProgramGenerator(x)).result) } distinct
    //val ress1 = (pts map { x => new NGenerator(x).result } distinct) sortWith { _.size < _.size }

    println(pts.size + " trees")
    println(ress.size + " programs")

    if (printResults) {
      {
        val initialTreeGrammar = varTreeGrammar.addRule(RuleName("R0"), target)
        val markedProgram = new ProgramMarked(program)
        val approximatedTreeGrammar = (new FlowAnalyser(markedProgram)).extP(initialTreeGrammar)
        val approximatedTreeGrammarCleanded = TreeGrammar.Cleaner.clean(approximatedTreeGrammar)
        
        println("False is reachable: " + FlowAnalyser.isReachable(SParsers.parsePat("False()"), RuleName("R0"), approximatedTreeGrammarCleanded))
        println("True is reachable: " + FlowAnalyser.isReachable(SParsers.parsePat("True()"), RuleName("R0"), approximatedTreeGrammarCleanded))
        println()
      }
      
      for ((resTerm, resProgram) <- ress) {
        val markedProgram = new ProgramMarked(resProgram)
        val initialTreeGrammar = varTreeGrammar.addRule(RuleName("R0"), resTerm)
        val approximatedTreeGrammar = (new FlowAnalyser(markedProgram)).extP(initialTreeGrammar)
        val approximatedTreeGrammarCleanded = TreeGrammar.Cleaner.clean(approximatedTreeGrammar)

        println("-------")
        println(resTerm)
        println()
        println(resProgram)
        println()
        println("Approximated " + approximatedTreeGrammarCleanded)
        println()
        println("False is reachable: " + FlowAnalyser.isReachable(SParsers.parsePat("False()"), RuleName("R0"), approximatedTreeGrammarCleanded))
        println("True is reachable: " + FlowAnalyser.isReachable(SParsers.parsePat("True()"), RuleName("R0"), approximatedTreeGrammarCleanded))
        println()
      }
    }
  }
}