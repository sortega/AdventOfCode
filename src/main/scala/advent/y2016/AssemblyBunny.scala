package advent.y2016

import scala.annotation.tailrec
import scala.util.parsing.combinator.RegexParsers

object AssemblyBunny {
  sealed trait Register
  object Register {
    case object A extends Register
    case object B extends Register
    case object C extends Register
    case object D extends Register
  }

  sealed trait Value
  case class Immediate(value: Int) extends Value
  case class Reference(register: Register) extends Value

  sealed trait Instruction
  case class CPY(from: Value, to: Value) extends Instruction
  case class INC(r: Register) extends Instruction
  case class DEC(r: Register) extends Instruction
  case class JNZ(cond: Value, offset: Value) extends Instruction
  case class TGL(r: Register) extends Instruction
  case class OUT(r: Register) extends Instruction

  object AssemblyGrammar extends RegexParsers {
    private def listing: Parser[Vector[Instruction]] = instruction.* ^^ { _.toVector }
    private def instruction: Parser[Instruction] = cpy | inc | dec | jnz | tgl | out
    private def cpy = "cpy" ~> value ~ value ^^ {
      case from ~ to => CPY(from, to)
    }
    private def value: Parser[Value] = number ^^ Immediate.apply | register ^^ Reference.apply
    private def number = """-?\d+""".r ^^ { _.toInt }
    private def register: Parser[Register] =
      "a" ^^^ Register.A | "b" ^^^ Register.B | "c" ^^^ Register.C | "d" ^^^ Register.D
    private def inc = "inc" ~> register ^^ INC.apply
    private def dec = "dec" ~> register ^^ DEC.apply
    private def jnz = "jnz" ~> value ~ value ^^ {
      case value ~ offset => JNZ(value, offset)
    }
    private def tgl = "tgl" ~> register ^^ TGL.apply
    private def out = "out" ~> register ^^ OUT.apply

    def parse(input: String): Vector[Instruction] = parseAll(listing, input).get
  }

  case class Computer(listing: IndexedSeq[Instruction],
                      registers: Map[Register, Int] = Map.empty.withDefaultValue(0),
                      programCounter: Int = 0,
                      output: Vector[Int] = Vector.empty) {
    @tailrec
    final def run: Computer = if (halted) this else step.run

    def states: Stream[Computer] = Stream.iterate(this)(_.step).takeWhile(!_.halted)

    def halted: Boolean = programCounter >= listing.size

    def step: Computer =
      listing(programCounter) match {
        case CPY(value, Reference(to)) => update(to, lookup(value)).updateProgramCounter()
        case CPY(_, Immediate(_)) => updateProgramCounter()
        case INC(r) => update(r)(_ + 1).updateProgramCounter()
        case DEC(r) => update(r)(_ - 1).updateProgramCounter()
        case JNZ(cond, offset) =>
          updateProgramCounter(if (lookup(cond) != 0) lookup(offset) else 1)
        case TGL(offsetRegister) =>
          val index = programCounter + registers(offsetRegister)
          if (listing.indices.contains(index)) {
            val newInstruction = listing(index) match {
              case INC(r) => DEC(r)
              case DEC(r) => INC(r)
              case TGL(r) => INC(r)
              case OUT(r) => INC(r)
              case JNZ(value, offset) => CPY(value, offset)
              case CPY(from, to) => JNZ(from, to)
            }
            copy(listing = listing.updated(index, newInstruction)).updateProgramCounter()
          } else updateProgramCounter()
        case OUT(r) => copy(output = output :+ registers(r)).updateProgramCounter()
      }

    private def lookup(v: Value): Int = v match {
      case Immediate(i) => i
      case Reference(r) => registers(r)
    }

    def updateProgramCounter(by: Int = 1): Computer = copy(programCounter = programCounter + by)
    def update(r: Register, v: Int): Computer = copy(registers = registers.updated(r, v))
    def update(r: Register)(f: Int => Int): Computer = update(r, f(registers(r)))

    def clearOutput: Computer = copy(output = Vector.empty)
  }

}
