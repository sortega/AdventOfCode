package advent.y2016

import advent.y2016.AssemblyBunny.{AssemblyGrammar, Computer, Register}

object Day23 {

  private def decode(input: String, n: Int) = {
    val listing = AssemblyGrammar.parse(input)
    Computer(listing).update(Register.A, n).run.registers(Register.A)
  }

  def part1(input: String): Int = decode(input, 7)

  def part2(input: String): Int = decode(input, 12)

  def main(args: Array[String]): Unit = {
    val input =
      """cpy a b
        |dec b
        |cpy a d
        |cpy 0 a
        |cpy b c
        |inc a
        |dec c
        |jnz c -2
        |dec d
        |jnz d -5
        |dec b
        |cpy b c
        |cpy c d
        |dec d
        |inc c
        |jnz d -2
        |tgl c
        |cpy -16 c
        |jnz 1 c
        |cpy 87 c
        |jnz 97 d
        |inc a
        |inc d
        |jnz d -2
        |inc c
        |jnz c -5
        |""".stripMargin
    timed(println("Part 1 result: " + part1(input)))
    timed(println("Part 2 result: " + part2(input)))
  }
}
