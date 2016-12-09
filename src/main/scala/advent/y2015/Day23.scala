package advent.y2015

import scala.annotation.tailrec
import scala.util.parsing.combinator.RegexParsers
import scalaz.syntax.std.boolean._

object Day23 {

  sealed trait Register
  object Register {
    case object A extends Register
    case object B extends Register
  }

  sealed trait Instruction {
    def execute(machine: Machine): Machine
  }

  object Instruction {
    case class HLF(r: Register) extends Instruction {
      override def execute(machine: Machine) = machine
          .modifyRegister(r)(_ / 2)
          .incrementPc()
    }

    case class TPL(r: Register) extends Instruction {
      override def execute(machine: Machine) = machine
        .modifyRegister(r)(_ * 3)
        .incrementPc()
    }

    case class INC(r: Register) extends Instruction {
      override def execute(machine: Machine) = machine
        .modifyRegister(r)(_ + 1)
        .incrementPc()
    }

    case class JMP(offset: Int) extends Instruction {
      override def execute(machine: Machine) = machine.incrementPc(offset)
    }

    case class JIE(r: Register, offset: Int) extends Instruction {
      override def execute(machine: Machine) =
        machine.incrementPc(if (machine(r) % 2 == 0) offset else 1)
    }

    case class JIO(r: Register, offset: Int) extends Instruction {
      override def execute(machine: Machine) =
        machine.incrementPc(if (machine(r) == 1) offset else 1)
    }
  }

  object Assembly extends RegexParsers {

    def parse(source: String) = parseAll(listing, source).get

    def listing: Parser[List[Instruction]] = instruction *

    def instruction = arithmetic | jump

    def arithmetic = (hlf | tpl | inc) ~ register ^^ {
      case operator ~ operand => operator(operand)
    }

    def hlf = "hlf" ^^^ Instruction.HLF.apply _
    def tpl = "tpl" ^^^ Instruction.TPL.apply _
    def inc = "inc" ^^^ Instruction.INC.apply _

    def jump = unconditionalJump | conditionalJump

    def unconditionalJump = "jmp" ~> offset ^^ Instruction.JMP.apply

    def conditionalJump =
      ("jie" ^^^ Instruction.JIE.apply _ |
        "jio" ^^^ Instruction.JIO.apply _) ~ register ~ "," ~ offset ^^ {
        case jump ~ register ~ _ ~ offset => jump(register, offset)
      }

    def offset = "(\\+|-)\\d+".r ^^ { _.toInt }

    def register = "a" ^^^ Register.A | "b" ^^^ Register.B
  }

  case class Machine(listing: List[Instruction], a: Int = 0, b: Int = 0, pc: Int = 0) {

    def apply(register: Register): Int = register match {
      case Register.A => a
      case Register.B => b
    }

    def incrementPc(offset: Int = 1) = copy(pc = pc + offset)

    def modifyRegister(register: Register)(f: Int => Int) = register match {
      case Register.A => copy(a = f(a))
      case Register.B => copy(b = f(b))
    }

    def fetch: Option[Instruction] = listing.indices.contains(pc).option(listing(pc))
  }

  @tailrec
  def runMachine(machine: Machine): Machine =
    machine.fetch match {
      case None => machine
      case Some(instruction) => runMachine(instruction.execute(machine))
    }

  def part1(input: String): Int = runMachine(Machine(Assembly.parse(input))).b

  def part2(input: String): Int = runMachine(Machine(Assembly.parse(input), a = 1)).b

  def main(args: Array[String]): Unit = {
    val input = """jio a, +18
                  |inc a
                  |tpl a
                  |inc a
                  |tpl a
                  |tpl a
                  |tpl a
                  |inc a
                  |tpl a
                  |inc a
                  |tpl a
                  |inc a
                  |inc a
                  |tpl a
                  |tpl a
                  |tpl a
                  |inc a
                  |jmp +22
                  |tpl a
                  |inc a
                  |tpl a
                  |inc a
                  |inc a
                  |tpl a
                  |inc a
                  |tpl a
                  |inc a
                  |inc a
                  |tpl a
                  |tpl a
                  |inc a
                  |inc a
                  |tpl a
                  |inc a
                  |inc a
                  |tpl a
                  |inc a
                  |inc a
                  |tpl a
                  |jio a, +8
                  |inc b
                  |jie a, +4
                  |tpl a
                  |inc a
                  |jmp +2
                  |hlf a
                  |jmp -7
                  |""".stripMargin
    println("Part 1 result: " + part1(input))
    println("Part 2 result: " + part2(input))
  }
}
