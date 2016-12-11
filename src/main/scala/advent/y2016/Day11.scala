package advent.y2016

import scalaz._
import Scalaz._
import scala.collection.immutable.HashSet
import scala.collection.parallel.immutable.ParSet

object Day11 {
  sealed abstract class Floor(value: Int) extends Product {
    override def toString: String = s"F$value"
    def contiguousTo: Set[Floor]
  }

  object Floor {
    case object F1 extends Floor(1) {
      override def contiguousTo = Set(F2)
    }
    case object F2 extends Floor(2) {
      override def contiguousTo = Set(F1, F3)
    }
    case object F3 extends Floor(3) {
      override def contiguousTo = Set(F2, F4)
    }
    case object F4 extends Floor(4) {
      override def contiguousTo = Set(F3)
    }

    val All = Seq[Floor](F1, F2, F3, F4)
  }

  sealed trait Machinery extends Product {
    val isotope: String
    override def toString: String = isotope.take(1).toUpperCase ++ productPrefix.take(1)
  }
  case class Generator(isotope: String) extends Machinery
  case class Microchip(isotope: String) extends Machinery

  case class Factory(elevator: Floor, floors: Map[Floor, Set[Machinery]]) {
    def isSafe: Boolean =
      Floor.All.forall { floor =>
        val machinery = floors(floor)
        val hasGenerators = machinery.exists {
          case _: Generator => true
          case _ => false
        }
        !hasGenerators || machinery.forall {
          case Microchip(isotope) => machinery.contains(Generator(isotope))
          case _ => true
        }
      }

    def isFinal: Boolean = (floors - Floor.F4).values.map(_.size).sum == 0

    def moveTo(destinationFloor: Floor, movedMachinery: Set[Machinery]): Factory =
      Factory(destinationFloor,
              floors ++ Map(
                elevator -> (floors(elevator) -- movedMachinery),
                destinationFloor -> (floors(destinationFloor) ++ movedMachinery)
              ))

    def transitions: Seq[Factory] = {
      val machinery = floors(elevator)
      for {
        destinationFloor <- elevator.contiguousTo.toSeq
        cargo1 <- machinery
        cargo2 <- machinery
        nextState = moveTo(destinationFloor, Set(cargo1, cargo2))
        if nextState.isSafe
      } yield nextState
    }

    def isotopes: Set[String] = floors.values.flatten.map(_.isotope).toSet

    def machinery: Seq[Machinery] =
      isotopes.toSeq.sorted.flatMap(isotope => Seq(Generator(isotope), Microchip(isotope)))

    override def toString: String = Floor.All.reverse.map(formatFloor).mkString("", "\n", "\n")

    private def formatFloor(floor: Floor) = {
      val floorSign = Some(floor)
      val elevatorSign = (elevator == floor).option("E")
      val presentMachinery = machinery.map(m => floors(floor).contains(m).option(m))
      formatLine(Seq(floorSign, elevatorSign) ++ presentMachinery)
    }

    private def formatLine(values: Seq[Option[Any]]): String =
      values.map { maybeValue =>
        maybeValue.fold(". ") { value =>
          value.toString.padTo(2, ' ')
        }
      }.mkString(" ").trim
  }

  case class StateSearch(aliveStates: ParSet[Factory], seenStates: ParSet[Factory] = ParSet.empty) {
    def step: StateSearch =
      StateSearch(aliveStates.flatMap(_.transitions) diff seenStates, seenStates = aliveStates)
    def found: Boolean = aliveStates.exists(_.isFinal)
  }

  object StateSearch {
    def initial(factory: Factory) = StateSearch(aliveStates = ParSet(factory))
  }

  def solve(factory: Factory): Int =
    Stream
      .iterate(StateSearch.initial(factory)) { _.step }
      .zipWithIndex
      .map {
        case (search, index) =>
          println(s"GENERATION $index: ${search.aliveStates.size} states")
          search
      }
      .indexWhere(_.found)

  def main(args: Array[String]): Unit = {
    val input1 = Factory(
      elevator = Floor.F1,
      floors = Map(
        Floor.F1 -> Set(Generator("polonium"),
                        Generator("thulium"),
                        Microchip("thulium"),
                        Generator("promethium"),
                        Generator("ruthenium"),
                        Microchip("ruthenium"),
                        Generator("cobalt"),
                        Microchip("cobalt")),
        Floor.F2 -> Set(Microchip("polonium"), Microchip("promethium")),
        Floor.F3 -> Set.empty,
        Floor.F4 -> Set.empty
      )
    )
    val input2 = input1.copy(
      floors = input1.floors |+| Map(
          Floor.F1 -> Set(Generator("elerium"),
                          Microchip("elerium"),
                          Generator("dilithium"),
                          Microchip("dilithium"))
        )
    )
    println("Part 1 result: " + solve(input1))
    println("Part 2 result: " + solve(input2))
  }
}
