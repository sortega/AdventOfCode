package advent.shared.discopt

import com.google.ortools.linearsolver.MPSolver

object SampleLinearSolver extends App {
  OrTools.ensureLoaded()

  val solver =
    new MPSolver("my_program", MPSolver.OptimizationProblemType.valueOf("GLOP_LINEAR_PROGRAMMING"))

  // Create the variables x and y.
  val x = solver.makeNumVar(0.0, 1.0, "x")
  val y = solver.makeNumVar(0.0, 2.0, "y")

  // Create the objective function, x + y.
  val objective = solver.objective
  objective.setCoefficient(y, 1)
  objective.setMaximization()

  // Call the solver and display the results.
  solver.solve()

  println("Solution:")
  println("x = " + x.solutionValue)
  println("y = " + y.solutionValue)
}
