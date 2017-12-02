package advent.y2015

import scala.language.postfixOps
import scala.util.parsing.combinator.RegexParsers
import scalaz.Scalaz._
import scalaz._

object Day23bis {

  sealed trait Register
  object Register {
    case object A extends Register
    case object B extends Register
  }

  case class Machine(listing: List[Instruction], a: Int = 0, b: Int = 0, pc: Int = 0) {

    def modifyRegister(register: Register)(f: Int => Int) = register match {
      case Register.A => copy(a = f(a))
      case Register.B => copy(b = f(b))
    }

    def fetch: Option[Instruction] = listing.indices.contains(pc).option(listing(pc))
  }

  type Statement[A] = State[Machine, A]

  object Statement {

    val noop: Statement[Unit] = ().pure[Statement]

    def jump(offset: Int): Statement[Unit] = State.modify[Machine] { machine =>
      machine.copy(pc = machine.pc + offset)
    }

    val jumpToNext: Statement[Unit] = jump(1)

    def read(register: Register): Statement[Int] =
      State.gets(machine => register match {
        case Register.A => machine.a
        case Register.B => machine.b
      })

    def write(register: Register, value: Int): Statement[Unit] = State.modify { machine =>
      register match {
        case Register.A => machine.copy(a = value)
        case Register.B => machine.copy(b = value)
      }
    }

    def modifyRegister(register: Register)(f: Int => Int): Statement[Unit] = for {
      value <- read(register)
      _ <- write(register, f(value))
      _ <- jumpToNext
    } yield ()

    def conditionalJump(register: Register, offset: Int)(f: Int => Boolean): Statement[Unit] = for {
      value <- read(register)
      _ <- jump(if (f(value)) offset else 1)
    } yield ()

    def fetchInstruction: Statement[Option[Instruction]] = State.gets { machine =>
      machine.listing.indices.contains(machine.pc)
        .option(machine.listing(machine.pc))
    }

    def executeInstruction: Statement[Unit] = ???
  }

  sealed trait Instruction {
    def toStatement: Statement[Unit]
  }

  object Instruction {
    import Statement._

    case class HLF(r: Register) extends Instruction {
      override def toStatement = modifyRegister(r)(_ / 2)
    }

    case class TPL(r: Register) extends Instruction {
      override def toStatement = modifyRegister(r)(_ * 3)
    }

    case class INC(r: Register) extends Instruction {
      override def toStatement = modifyRegister(r)(_ + 1)
    }

    case class JMP(offset: Int) extends Instruction {
      override def toStatement = jump(offset)
    }

    case class JIE(r: Register, offset: Int) extends Instruction {
      override def toStatement = conditionalJump(r, offset)(_ % 2 == 0)
    }

    case class JIO(r: Register, offset: Int) extends Instruction {
      override def toStatement = conditionalJump(r, offset)(_ == 1)
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

  def runMachine(machine: Machine): Machine = {
    val step: Statement[Boolean] = for {
      instruction <- Statement.fetchInstruction
      _ <- instruction.fold(Statement.noop)(_.toStatement)
    } yield instruction.isDefined

    step.iterateWhile(identity).run(machine)._1
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
