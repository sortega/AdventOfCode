package advent.shared.discopt

import com.google.ortools.constraintsolver._
import scalaz.Scalaz._

final case class Passenger(id: Int, prefersWindow: Boolean = false)

// TODO: what if there are not enough passengers? -> all different or -1
// TODO: what if there is overbooking?
final class PlainSolver(numRows: Int, seatsPerRow: Int, groups: List[List[Passenger]]) {
  OrTools.ensureLoaded()

  private val totalSeats: Int              = numRows * seatsPerRow
  private val passengers: Array[Passenger] = groups.flatten.toArray
  private val passengerIds: Array[Int]     = groups.flatMap(_.map(_.id)).toArray
  private val passengerGroups: Array[Int] = for {
    (group, groupId) <- groups.zipWithIndex.toArray
    _                <- group
  } yield groupId

  def computeLayout(): (List[List[Int]], Int) = {
    val solver = new Solver("plain solver")

    // Model constraints
    // Absolute seat each passenger has
    val passengerSeats = solver.makeIntVarArray(passengerIds.length, 0, totalSeats - 1, "seats")
    solver.addConstraint(solver.makeAllDifferent(passengerSeats))
    val passengerRows = solver.makeIntVarArray(passengerIds.length, 0, numRows - 1, "rows")
    val passengerCols = solver.makeIntVarArray(passengerIds.length, 0, seatsPerRow - 1, "cols")

    // Tie rows to seats
    for ((seat, row, col) <- (passengerSeats, passengerRows, passengerCols).zipped) {
      solver.addConstraint(
        solver.makeScalProdEquality(Array(row, col), Array(seatsPerRow, 1), seat))
    }

    // One variable per group
    val groupSatisfaction: Array[IntVar] = for {
      groupIndex <- groups.indices.toArray
    } yield {
      val rowsOfPeopleInTheGroup = passengerRows.zip(passengerGroups).collect {
        case (row, `groupIndex`) => row
      }
      if (rowsOfPeopleInTheGroup.length < 2) solver.makeTrueConstraint().`var`()
      else
        solver
          .makeMin(
            rowsOfPeopleInTheGroup
              .sliding(size = 2, step = 1)
              .collect {
                case Array(left, right) =>
                  solver.makeIsEqualVar(left, right)
              }
              .toArray)
          .`var`()
    }

    // Passenger satisfaction
    val satisfactionConstraints: Array[IntVar] = for {
      ((passenger, groupIndex), passengerIndex) <- passengers.zip(passengerGroups).zipWithIndex
    } yield {
      if (!passenger.prefersWindow) groupSatisfaction(groupIndex)
      else
        solver
          .makeMin(
            Array(
              groupSatisfaction(groupIndex),
              solver
                .makeMax(
                  Array(solver.makeIsEqualCstVar(passengerCols(passengerIndex), 0),
                        solver.makeIsEqualCstVar(passengerCols(passengerIndex), seatsPerRow - 1)))
                .`var`()
            )
          )
          .`var`()
    }

    val totalSatisfaction = solver.makeSum(satisfactionConstraints).`var`()
    val opt               = solver.makeMaximize(totalSatisfaction, 1)

    // Search for the best solution
    val allVariables = passengerSeats ++ passengerRows ++ passengerCols :+ totalSatisfaction
    val decisionBuilder =
      solver.makePhase(allVariables, Solver.CHOOSE_MIN_SIZE_LOWEST_MIN, Solver.ASSIGN_MIN_VALUE)

    val collector = solver.makeLastSolutionCollector()
    collector.add(passengerSeats)
    collector.addObjective(totalSatisfaction)

    val log = solver.makeSearchLog(10000, opt)
    solver.solve(decisionBuilder, opt, log, collector)

    // Convert into a solution
    val passengersBySeat: Map[Int, Int] = passengerSeats.zipWithIndex.toList.foldMap {
      case (seatVar, index) =>
        Map(collector.value(0, seatVar).toInt -> passengerIds(index))
    }
    val layout = List.tabulate(numRows, seatsPerRow) { (rowIndex, colIndex) =>
      passengersBySeat(rowIndex * seatsPerRow + colIndex)
    }
    val satisfiedPeople = collector.objectiveValue(0).toInt
    (layout, satisfiedPeople)
  }
}

object PlainSolver extends App {
  // TODO: I/O stuff
  args match {
    case Array(filename) => // parse
    case _               => // explode
  }

  // invoke computeLayout

  // format result
}
