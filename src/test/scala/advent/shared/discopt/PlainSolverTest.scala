package advent.shared.discopt

import advent.shared.test.UnitTest

final class PlainSolverTest extends UnitTest {

  "The plain satisfaction solver" should "get full marks for the example problem" in {
    val solver = new PlainSolver(
      numRows = 4,
      seatsPerRow = 4,
      groups = List(
        List(Passenger(1, prefersWindow = true), Passenger(2), Passenger(3)),
        List(Passenger(4), Passenger(5), Passenger(6), Passenger(7)),
        List(Passenger(8)),
        List(Passenger(9), Passenger(10), Passenger(11, prefersWindow = true)),
        List(Passenger(12, prefersWindow = true)),
        List(Passenger(13), Passenger(14)),
        List(Passenger(15), Passenger(16))
      )
    )

    val (layout, satisfiedPassengers) = solver.computeLayout()

    satisfiedPassengers should ===(16)
    layout.foreach { row =>
      row.foreach { passenger =>
        print(passenger + " ")
      }
      println()
    }
  }
}

/*
1 2 3 8
4 5 6 7
11 9 10 12
13 14 15 16
100%
 */
