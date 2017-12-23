package advent.y2017

import scala.collection.parallel.ForkJoinTaskSupport

import advent.shared.Functions
import advent.shared.Time.timed
import scalaz._
import Scalaz._

import advent.shared.geom.Point

object Day21 {

  private val threadPool = new ForkJoinTaskSupport(new java.util.concurrent.ForkJoinPool(8))

  case class Pattern(size: Int, pixels: Set[Point]) {
    lazy val variants: Set[Pattern] = {
      val rotations = List.iterate(this, 4)(_.rotate)
      val flips     = rotations.map(_.flip)
      (rotations ++ flips).toSet
    }

    def rotate: Pattern = transform { pixel =>
      Point(y = pixel.x, x = size - pixel.y - 1)
    }

    def flip: Pattern = transform { pixel =>
      pixel.copy(y = size - pixel.y - 1)
    }

    def tiles(tileSize: Int): List[List[Pattern]] = {
      val tilesPerRow = size / tileSize
      (0 until tilesPerRow).toList.map { tileRow =>
        (0 until tilesPerRow).toList.map { tileCol =>
          Pattern(tileSize, pixels.collect {
            case pixel if pixel.y / tileSize == tileRow && pixel.x / tileSize == tileCol =>
              Point(y = pixel.y % tileSize, x = pixel.x % tileSize)
          })
        }
      }
    }

    def mapTiles(tileSize: Int)(f: Pattern => Pattern): Pattern =
      Pattern.untile {
        val parTiles = tiles(tileSize).par
        parTiles.tasksupport = threadPool
        parTiles.map(_.map(f)).toList
      }

    private def transform(f: Point => Point) = copy(pixels = pixels.map(f))

    override def toString: String =
      (0 until size)
        .map { row =>
          (0 until size).map { col =>
            if (pixels.contains(Point(col, row))) '#' else '.'
          }.mkString
        }
        .mkString("/")
  }

  object Pattern {
    val Initial: Pattern = parse(".#./..#/###")

    def parse(input: String): Pattern = {
      val lines = input.split("/").toList
      val pixels = for {
        (line, row) <- lines.zipWithIndex
        (char, col) <- line.zipWithIndex
        if char == '#'
      } yield Point(y = row, x = col)
      Pattern(lines.size, pixels = pixels.toSet)
    }

    def untile(tile: List[List[Pattern]]): Pattern = {
      val cellSize = tile.head.head.size
      val pixels = for {
        (line, tileRow) <- tile.zipWithIndex
        (cell, tileCol) <- line.zipWithIndex
        base = Point(y = cellSize * tileRow, x = cellSize * tileCol)
        pixel <- cell.pixels
      } yield base + pixel
      Pattern(tile.size * cellSize, pixels.toSet)
    }
  }

  case class Rule(from: Pattern, to: Pattern) {
    def matches(pattern: Pattern): Boolean = from.variants.contains(pattern)
  }

  object Rule {
    private val Format = "([./#]+) => ([./#]+)".r

    def parse(input: String): Rule = input match {
      case Format(from, to) => Rule(Pattern.parse(from), Pattern.parse(to))
    }
  }

  case class RuleBook(rules: List[Rule]) {
    def transform(pattern: Pattern): Pattern =
      if (pattern.size <= 3) transformTile(pattern)
      else pattern.mapTiles(tileSizeFor(pattern))(transformTile)

    private def transformTile(pattern: Pattern) =
      rules
        .find(_.matches(pattern))
        .getOrElse(throw new IllegalArgumentException(s"no rule matches $pattern"))
        .to

    private def tileSizeFor(pattern: Pattern): Int = if (pattern.size % 2 == 0) 2 else 3
  }

  object RuleBook {
    def parse(input: String): RuleBook = RuleBook(input.lines.map(Rule.parse).toList)
  }

  private def pixelsAfterIterations(input: String, iterations: Int) = {
    val rulebook = RuleBook.parse(input)
    Functions.repeatedly(iterations)(rulebook.transform).apply(Pattern.Initial).pixels.size
  }

  def part1(input: String): Int = pixelsAfterIterations(input, 5)

  def part2(input: String): Int = {
    val rulebook   = RuleBook.parse(input)
    val transform3 = Functions.repeatedly(3)(rulebook.transform)

    val initialTiles = Map(Pattern.Initial -> 1)

    val tiles = Stream
      .iterate(initialTiles) { tiles =>
        tiles.toList.foldMap {
          case (tile, count) =>
            transform3(tile).tiles(3).flatten.foldMap { transformedTile =>
              Map(transformedTile -> count)
            }
        }
      }
      .drop(6)
      .head

    tiles.map {
      case (tile, count) =>
        tile.pixels.size * count
    }.sum
  }

  def main(args: Array[String]): Unit = {
    val input = inputResource(day = 21).getLines().mkString("\n")
    timed(println("Part 1 result: " + part1(input)))
    timed(println("Part 2 result: " + part2(input)))
  }
}
