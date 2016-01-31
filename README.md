# Level your Scala up

This repo has a template project for the deliberate practice session of Scala
using the [advent of code]. We will have a private leaderboard, ask for the
joining code during the session.

[advent]: http://adventofcode.com/

The objective of this activity is to *improve at writing Scala code* by
writing non-production code at shouting distance of more experienced
developers. We will circulating the room and suggesting improvements and
things to try but don't hesitate to ask!

## Requirements

- You should be able to write some Scala code to begin with. This is not a
  feel-good, nod-your-head session. Take a look at the introduction video of
  @apoloval or read [an introductory tutorial][intro].
- You will need a recent JVM (7 or 8) and [SBT][sbt].
- While a proper IDE is not mandatory, at least an editor with syntax
  highlighting is recommended. My recommendation of IDE is the
  [IntelliJ][idea] community edition with the Scala plugin (Settings, Plugins,
  Browse repositories, and then select the Scala one).

[intro]: https://www.google.es/#q=intro+to+scala
[sbt]: http://www.scala-sbt.org/
[idea]: https://www.jetbrains.com/idea/download/index.html

## How to use this repo

First, either clone it with `git clone
git@github.com:sortega/AdventOfCode.git` or download a [zipped version][zip].

[zip]: https://github.com/sortega/AdventOfCode/archive/master.zip

Advent Of Code is organized in days, having two related problems to solve per
day. You will find skeleton files for the first ones in
`src/main/scala/advent`, named `DayX.scala`. You are supposed to write the
implementations for the methods `part1` and `part2` (feel free of adding
auxiliar functions and classes).

```scala
package advent

object Day1 {

  def part1(input: String): Int = ???

  def part2(input: String): Int = ???

  def main(args: Array[String]): Unit = {
    val basicInput = ""
    val difficultInput = ""
    println("Part 1 result: " + part1(basicInput))
    println("Part 2 result: " + part2(difficultInput))
  }
}
```

There are some sample tests in `src/test/scala/advent` that you might want to
use and extends with additional test cases. The easiest way to execute then is
opening a SBT session from the project root:

```console
% sbt
[info] Loading project definition from /Users/sortega/Repositories/github/AdventOfCode/project
[info] Set current project to advent-of-code (in build file:/Users/sortega/Repositories/github/AdventOfCode/)
> test
[info] Compiling 1 Scala source to /Users/sortega/Repositories/github/AdventOfCode/target/scala-2.11/classes...
[info] Compiling 1 Scala source to /Users/sortega/Repositories/github/AdventOfCode/target/scala-2.11/test-classes...
[info] Day1Test:
...
[error] Failed: Total 52, Failed 49, Errors 0, Passed 3
...
> testOnly advent.Day1Test
[info] Day1Test:
...
[info] Total number of tests run: 5
[info] Suites: completed 1, aborted 0
[info] Tests: succeeded 0, failed 5, canceled 0, ignored 0, pending 0
[info] *** 5 TESTS FAILED ***
```

Use `test` for running all the tests and `testOnly advent.DayXXXTest` for
focusing on one of them. You can turn the test to auto run by prepending a
tilde (`~test` instead of `test`).

The code skeletons have a main function for producing the final result that
Advent Of Code site asks for. If you copy the problem input to the
corresponding variable (the input is different for each participant) you can
get the results this way:

```console
% sbt
[info] Loading project definition from /Users/sortega/Repositories/github/AdventOfCode/project
[info] Set current project to advent-of-code (in build file:/Users/sortega/Repositories/github/AdventOfCode/)
> runMain advent.DayXX
Part 1 result: XXXX
Part 2 result: XXXX
```
