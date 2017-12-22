package advent.y2017

import advent.shared.Point
import advent.y2017.Day21.{Pattern, Rule, RuleBook}
import org.scalatest.{FlatSpec, Matchers}

class Day21Test extends FlatSpec with Matchers {

  "A pattern" should "be rotated" in {
    Pattern.parse("#./.#").rotate shouldBe Pattern.parse(".#/#.")
    Pattern.parse("###/##./.#.").rotate shouldBe Pattern.parse(".##/###/..#")
  }

  it should "be vertically flipped" in {
    Pattern.parse("#./.#").flip shouldBe Pattern.parse(".#/#.")
    Pattern.parse("###/##./.#.").flip shouldBe Pattern.parse(".#./##./###")
  }

  it should "be split in tiles" in {
    Pattern.parse("#..#/..../..../#..#").tiles(tileSize = 2) shouldBe List(
      List(Pattern.parse("#./.."), Pattern.parse(".#/..")),
      List(Pattern.parse("../#."), Pattern.parse("../.#"))
    )
  }

  it should "be recomposed from tiles" in {
    Pattern.untile(
      List(
        List(Pattern.parse("#./.."), Pattern.parse(".#/..")),
        List(Pattern.parse("../#."), Pattern.parse("../.#"))
      )
    ) shouldBe Pattern.parse("#..#/..../..../#..#")
  }

  "A rule" should "be parsed" in {
    Rule.parse("#./.. => ###/##./.#.") shouldBe
      Rule(
        from = Pattern(size = 2, pixels = Set(Point(0, 0))),
        to = Pattern(size = 3,
                     pixels = Set(
                       Point(y = 0, x = 0),
                       Point(y = 0, x = 1),
                       Point(y = 0, x = 2),
                       Point(y = 1, x = 0),
                       Point(y = 1, x = 1),
                       Point(y = 2, x = 1)
                     ))
      )
  }

  it should "check if matches against a pattern" in {
    val rule = Rule.parse("../.# => ##./#../...")
    rule.matches(Pattern.parse("../.#")) shouldBe true
    rule.matches(Pattern.parse("#./..")) shouldBe true
    rule.matches(Pattern.parse("#./.#")) shouldBe false
    rule.matches(Pattern.parse("#../.../...")) shouldBe false
  }

  val rulebook = RuleBook.parse("""../.# => ##./#../...
                                  |.#./..#/### => #..#/..../..../#..#
                                  |""".stripMargin)

  "A rule book" should "transform a small pattern" in {
    rulebook.transform(Pattern.parse("../.#")) shouldBe Pattern.parse("##./#../...")
    rulebook.transform(Pattern.parse(".#./..#/###")) shouldBe Pattern.parse("#..#/..../..../#..#")
  }

  it should "transform a big pattern in a tile-wise way" in {
    rulebook.transform(Pattern.parse("#..#/..../..../#..#")) shouldBe
      Pattern.parse("##.##./#..#../....../##.##./#..#../......")
  }

  "Part 1" should "count the number of pixels after 5 iterations" in {
    Day21.part1("""../.# => ##./#../...
                  |##/#. => .#./#../...
                  |.#/.# => .../.../...
                  |../.. => ../..
                  |.#./..#/### => #..#/..../..../#..#
                  |.#./#../... => ..../..../..../....
                  |.../.../... => ..../..../..../....
                  |##./#../... => ..../#.../...#/....
                  |""".stripMargin) shouldBe 30
  }

  "Part 2" should "count the number of pixels after 18 iterations" in {
    Day21.part2("""../.# => ##./#../...
                  |##/#. => .#./#../...
                  |.#/.# => .../.../...
                  |../.. => .../.../...
                  |.#/#. => .#./#../...
                  |.#./..#/### => #..#/..../..../#..#
                  |.#./#../... => ..../..../..../....
                  |.../.../... => ..../..../..../....
                  |##./#../... => ..../#.../...#/....
                  |""".stripMargin) shouldBe 4455

  }
}
