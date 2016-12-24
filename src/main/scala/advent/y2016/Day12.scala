package advent.y2016

import advent.y2016.AssemblyBunny._

object Day12 {

  def part1(input: String): Int = Computer(AssemblyGrammar.parse(input)).run.registers(Register.A)

  def part2(input: String): Int = {
    val initialRegisters = Map[Register, Int](Register.C -> 1).withDefaultValue(0)
    Computer(AssemblyGrammar.parse(input), initialRegisters).run.registers(Register.A)
  }

  def main(args: Array[String]): Unit = {
    val input = """cpy 1 a
                  |cpy 1 b
                  |cpy 26 d
                  |jnz c 2
                  |jnz 1 5
                  |cpy 7 c
                  |inc d
                  |dec c
                  |jnz c -2
                  |cpy a c
                  |inc a
                  |dec b
                  |jnz b -2
                  |cpy c b
                  |dec d
                  |jnz d -6
                  |cpy 14 c
                  |cpy 14 d
                  |inc a
                  |dec d
                  |jnz d -2
                  |dec c
                  |jnz c -5
                  |""".stripMargin
    println("Part 1 result: " + part1(input))
    println("Part 2 result: " + part2(input))
  }
}
