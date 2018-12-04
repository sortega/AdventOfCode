package advent.y2018

import java.time.{LocalDate, LocalDateTime, ZoneOffset}
import java.time.format.{DateTimeFormatter, DateTimeFormatterBuilder}
import scalaz.Scalaz._
import scalaz.Semigroup

import advent.shared.Time.timed

object Day4 {
  type Minute  = Int
  type GuardId = Int

  sealed trait Event

  object Event {
    final case class ShiftStarts(guard: GuardId) extends Event
    case object FallsAsleep                      extends Event
    case object WakesUp                          extends Event

    private val ShiftStartsPattern = """Guard #(\d+) begins shift""".r

    def parse(s: String): Event = s match {
      case "wakes up"                => WakesUp
      case "falls asleep"            => FallsAsleep
      case ShiftStartsPattern(guard) => ShiftStarts(guard.toInt)
    }
  }

  final case class LogEntry(timestamp: LocalDateTime, event: Event)
  object LogEntry {
    private val Pattern = """\[(.*)\] (.*)""".r
    private val ElfTime = new DateTimeFormatterBuilder().parseCaseInsensitive
      .append(DateTimeFormatter.ISO_LOCAL_DATE)
      .appendLiteral(' ')
      .append(DateTimeFormatter.ISO_LOCAL_TIME)
      .toFormatter()

    def parse(s: String): LogEntry = s match {
      case Pattern(timestamp, event) =>
        LogEntry(LocalDateTime.parse(timestamp, ElfTime), Event.parse(event))
    }
  }

  type Log = List[LogEntry]
  object Log {
    def parse(lines: List[String]): Log =
      lines
        .map(LogEntry.parse)
        .sortBy(_.timestamp.toEpochSecond(ZoneOffset.UTC))
  }

  final case class ServiceSheet(minutesSlept: Map[LocalDate, Set[Minute]]) extends AnyVal {
    def mostSleptMinute: Minute =
      minutesSlept.values.flatten.groupBy(identity).mapValues(_.size).toList.maxBy(_._2)._1

    def totalMinutesSlept: Int = minutesSlept.values.map(_.size).sum
  }

  object ServiceSheet {
    val Empty = ServiceSheet(Map.empty)

    implicit object SheetSemigroup extends Semigroup[ServiceSheet] {
      override def append(left: ServiceSheet, right: => ServiceSheet): ServiceSheet =
        ServiceSheet(left.minutesSlept |+| right.minutesSlept)
    }

    def sleptDuring(date: LocalDate, minutes: Seq[Minute]): ServiceSheet =
      ServiceSheet(Map(date -> minutes.toSet))
  }

  type ServiceSheets = Map[GuardId, ServiceSheet]
  object ServiceSheets {
    final case class ServiceSheetBuilder(date: LocalDate,
                                         awake: Boolean,
                                         sinceMinute: Minute,
                                         partialSheet: ServiceSheet = ServiceSheet.Empty) {

      def awokeAt(minute: Minute): ServiceSheetBuilder =
        if (!awake)
          copy(awake = true,
               sinceMinute = minute,
               partialSheet =
                 partialSheet |+| ServiceSheet.sleptDuring(date, sinceMinute until minute))
        else this

      def fallenAsleepAt(minute: Minute): ServiceSheetBuilder =
        if (awake) copy(awake = false, sinceMinute = minute) else this

      def complete: ServiceSheet = awokeAt(minute = 60).partialSheet
    }

    object ServiceSheetBuilder {
      def from(timestamp: LocalDateTime): ServiceSheetBuilder =
        ServiceSheetBuilder(timestamp.toLocalDate, awake = true, sinceMinute = timestamp.getMinute)
    }

    final case class ServiceSheetsBuilder(guard: GuardId,
                                          sheetBuilder: ServiceSheetBuilder,
                                          partialSheets: ServiceSheets = Map.empty) {
      def addEntry(logEntry: LogEntry): ServiceSheetsBuilder = logEntry match {
        case LogEntry(timestamp, Event.ShiftStarts(anotherGuard)) =>
          copy(guard = anotherGuard,
               sheetBuilder = ServiceSheetBuilder.from(timestamp),
               partialSheets = partialSheets |+| Map(guard -> sheetBuilder.complete))

        case LogEntry(timestamp, Event.WakesUp) =>
          copy(sheetBuilder = sheetBuilder.awokeAt(timestamp.getMinute))

        case LogEntry(timestamp, Event.FallsAsleep) =>
          copy(sheetBuilder = sheetBuilder.fallenAsleepAt(timestamp.getMinute))
      }

      def complete: ServiceSheets = partialSheets |+| Map(guard -> sheetBuilder.complete)
    }

    def compile(log: Log): ServiceSheets =
      log match {
        case LogEntry(timestamp, Event.ShiftStarts(guard)) :: otherEntries =>
          val builder = ServiceSheetsBuilder(guard, ServiceSheetBuilder.from(timestamp))
          otherEntries.foldLeft(builder)(_.addEntry(_)).complete

        case unexpected => throw new IllegalStateException(s"Log cannot start with $unexpected")
      }
  }

  def part1(log: Log): Int = {
    val sheets               = ServiceSheets.compile(log)
    val guardSleepingTheMost = findGuardSleepingTheMost(sheets)
    val sheet                = sheets(guardSleepingTheMost)
    val mostSleptMinute      = sheet.mostSleptMinute
    guardSleepingTheMost * mostSleptMinute
  }

  private def findGuardSleepingTheMost(sheets: ServiceSheets): GuardId =
    sheets.keySet.maximumBy(guard => sheets(guard).totalMinutesSlept).get

  def part2(log: Log): Int = {
    val sheets                = ServiceSheets.compile(log)
    val timesByGuardAndMinute = timesSleptByGuardAndMinute(sheets)
    val (guard, minute)       = timesByGuardAndMinute.toList.maxBy(_._2)._1
    guard * minute
  }

  private def timesSleptByGuardAndMinute(sheets: ServiceSheets): Map[(GuardId, Minute), Int] =
    (for {
      (guard, sheet) <- sheets.toList
      minute         <- sheet.minutesSlept.values.flatten
    } yield (guard, minute)).groupBy(identity).mapValues(_.size)

  def main(args: Array[String]): Unit = {
    val input = Log.parse(inputResource(day = 4).getLines().toList)
    timed(println("Part 1 result: " + part1(input)))
    timed(println("Part 2 result: " + part2(input)))
  }
}
