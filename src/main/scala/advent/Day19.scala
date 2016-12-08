package advent

import scala.annotation.tailrec

object Day19 {

  case class Rule(pattern: String, replacement: String) {

    def swap = Rule(pattern = replacement, replacement = pattern)

    def apply(molecule: String): Set[String] =
      indicesOf(pattern, molecule).map { index =>
        val (prefix, suffix) = molecule.splitAt(index)
        prefix + suffix.replaceFirst(pattern, replacement)
      }

    private def indicesOf(pattern: String, text: String): Set[Int] = {
      @tailrec
      def indicesOfFrom(from: Int, indices: Set[Int]): Set[Int] =
        text.indexOf(pattern, from) match {
          case -1 => indices
          case index => indicesOfFrom(index + 1, indices + index)
        }
      indicesOfFrom(0, Set.empty)
    }
  }

  case class Rules(entries: Set[Rule]) {
    def swap = Rules(entries.map(_.swap))
    def apply(molecules: Set[String]): Set[String] = molecules.flatMap(apply)
    def apply(molecule: String): Set[String] = entries.flatMap(_.apply(molecule))
  }

  case class Configuration(molecule: String, rewriteRules: Rules) {

    def calibrate: Int = rewriteRules.apply(molecule).size

    def steps: Int = {
      val reverseRules = rewriteRules.swap
      Stream.iterate(Set(molecule))(reverseRules.apply)
        .indexWhere(_.contains("e"))
    }

  }

  object Configuration {
    private val RulePattern = """(\w+) => (\w+)""".r

    def parse(input: String): Configuration = {
      Configuration(
        molecule = input.trim.lines.toSeq.last,
        rewriteRules = Rules(RulePattern.findAllMatchIn(input)
          .map { ruleMatch => Rule(ruleMatch.group(1), ruleMatch.group(2)) }
          .toSet
        )
      )
    }
  }

  def part1(input: String): Int = Configuration.parse(input).calibrate

  def part2(input: String): Int = Configuration.parse(input).steps

  def main(args: Array[String]): Unit = {
    val input =
      """Al => ThF
        |Al => ThRnFAr
        |B => BCa
        |B => TiB
        |B => TiRnFAr
        |Ca => CaCa
        |Ca => PB
        |Ca => PRnFAr
        |Ca => SiRnFYFAr
        |Ca => SiRnMgAr
        |Ca => SiTh
        |F => CaF
        |F => PMg
        |F => SiAl
        |H => CRnAlAr
        |H => CRnFYFYFAr
        |H => CRnFYMgAr
        |H => CRnMgYFAr
        |H => HCa
        |H => NRnFYFAr
        |H => NRnMgAr
        |H => NTh
        |H => OB
        |H => ORnFAr
        |Mg => BF
        |Mg => TiMg
        |N => CRnFAr
        |N => HSi
        |O => CRnFYFAr
        |O => CRnMgAr
        |O => HP
        |O => NRnFAr
        |O => OTi
        |P => CaP
        |P => PTi
        |P => SiRnFAr
        |Si => CaSi
        |Th => ThCa
        |Ti => BP
        |Ti => TiTi
        |e => HF
        |e => NAl
        |e => OMg
        |
        |ORnPBPMgArCaCaCaSiThCaCaSiThCaCaPBSiRnFArRnFArCaCaSiThCaCaSiThCaCaCaCaCaCaSiRnFYFArSiRnMgArCaSiRnPTiTiBFYPBFArSiRnCaSiRnTiRnFArSiAlArPTiBPTiRnCaSiAlArCaPTiTiBPMgYFArPTiRnFArSiRnCaCaFArRnCaFArCaSiRnSiRnMgArFYCaSiRnMgArCaCaSiThPRnFArPBCaSiRnMgArCaCaSiThCaSiRnTiMgArFArSiThSiThCaCaSiRnMgArCaCaSiRnFArTiBPTiRnCaSiAlArCaPTiRnFArPBPBCaCaSiThCaPBSiThPRnFArSiThCaSiThCaSiThCaPTiBSiRnFYFArCaCaPRnFArPBCaCaPBSiRnTiRnFArCaPRnFArSiRnCaCaCaSiThCaRnCaFArYCaSiRnFArBCaCaCaSiThFArPBFArCaSiRnFArRnCaCaCaFArSiRnFArTiRnPMgArF
      """.stripMargin
    println("Part 1 result: " + part1(input))
    println("Part 2 result: " + part2(input))
  }
}
