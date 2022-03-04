package workshop

import zio._

object Retry extends ZIOAppDefault {

  // Schedule[-Env, -In, +Out]

  // ZIO[R, E, A]

  // Repeating
  // Schedule[R, A, B]

  // Retrying
  // Schedule[R, E, A]

  object ScheduleImplementation {
    import Schedule._
    import java.time.OffsetDateTime

    trait Schedule[-Env, -In, +Out] {

      type State

      def initial: State

      def step(now: OffsetDateTime, in: In, state: State)(
        implicit
        trace: ZTraceElement
      ): ZIO[Env, Nothing, (State, Out, Decision)]
    }

    object Schedule {

      sealed trait Decision

      object Decision {
        final case class Continue(interval: Interval) extends Decision
        case object Done                              extends Decision
      }

      sealed abstract class Interval private (val start: OffsetDateTime, val end: OffsetDateTime)
    }
  }

  /**
   * EXERCISE
   *
   * Using `Schedule.recurs`, create a schedule that recurs 5 times.
   */
  val fiveTimes: Schedule[Any, Any, Long] =
    Schedule.recurs(5)

  // ZIO.repeat
  // ZIO.retry

  /**
   * EXERCISE
   *
   * Using the `ZIO.repeat`, repeat printing "Hello World" five times to the
   * console.
   */
  val repeated1 =
    Console.printLine("Hello World").repeat(fiveTimes)

// Hello World
// Hello World
// Hello World
// Hello World
// Hello World
// Hello World

  /**
   * EXERCISE
   *
   * Using `Schedule.spaced`, create a schedule that recurs forever every 1 second.
   */
  lazy val everySecond =
    Schedule.spaced(1.second)

  /**
   * EXERCISE
   *
   * Using the `&&` method of the `Schedule` object, the `fiveTimes` schedule,
   * and the `everySecond` schedule, create a schedule that repeats fives times,
   * evey second.
   */
  lazy val fiveTimesEverySecond =
    fiveTimes && everySecond

  // def repeat[R, E, A](
  //   numberOfRepetitions: Int,
  //   durationBetweenRepetitions: Duration
  // )(zio: ZIO[R, E, A]) =
  //   ???

  /**
   * EXERCISE
   *
   * Using the `ZIO#repeat`, repeat the action printLine("Hi hi") using
   * `fiveTimesEverySecond`.
   */
  lazy val repeated2 =
    Console.printLine("Hi hi").repeat(fiveTimesEverySecond)

// Hi hi
// Hi hi
// Hi hi
// Hi hi
// Hi hi
// Hi hi

  /**
   * EXERCISE
   *
   * Using `Schedule#andThen` the `fiveTimes` schedule, and the `everySecond`
   * schedule, create a schedule that repeats fives times rapidly, and then
   * repeats every second forever.
   */
  lazy val fiveTimesThenEverySecond =
    fiveTimes andThen everySecond

  /**
   * EXERCISE
   *
   * Using `ZIO#retry`, retry the following error a total of five times.
   */
  val error1 = ZIO.debug("About to do something that might fail") *> IO.fail("Uh oh!")
  lazy val retried5 =
    error1.retry(fiveTimes)

  // val run =
  //   retried5

// [info] About to do something that might fail
// [info] About to do something that might fail
// [info] About to do something that might fail
// [info] About to do something that might fail
// [info] About to do something that might fail
// [info] About to do something that might fail

// [info] timestamp=2022-03-02T17:44:17.402324Z level=ERROR thread=#zio-fiber-0 message="Exception in thread "zio-fiber-2" java.lang.String: Uh oh!
// [info]  at workshop.Retry.retried5(06-schedule.scala:129)"

  /**
   * EXERCISE
   *
   * Using the `Schedule#||`, the `fiveTimes` schedule, and the `everySecond`
   * schedule, create a schedule that repeats the minimum of five times and
   * every second.
   */
  lazy val fiveTimesOrEverySecond =
    fiveTimes || everySecond

  /**
   * EXERCISE
   *
   * Using `Schedule.exponential`, create an exponential schedule that starts
   * from 10 milliseconds.
   */
  lazy val exponentialSchedule =
    Schedule.exponential(10.millis)

  // (effect orElse otherService).retry(exponentialSchedule).timeout(60.seconds)

  /**
   * EXERCISE
   *
   * Using `Schedule.jittered` produced a jittered version of `exponentialSchedule`.
   */
  lazy val jitteredExponential =
    exponentialSchedule.jittered

  /**
   * EXERCISE
   *
   * Using `Schedule.whileOutput`, produce a filtered schedule from `Schedule.forever`
   * that will halt when the number of recurrences exceeds 100.
   */
  lazy val oneHundred =
    Schedule.forever.whileOutput(_ < 100)

  val run =
    Console.printLine("Hello World").repeat(oneHundred)

  /**
   * EXERCISE
   *
   * Using `Schedule.identity`, produce a schedule that recurs forever, without delay,
   * returning its inputs.
   */
  def inputs[A]: Schedule[Any, A, A] =
    Schedule.identity

  /**
   * EXERCISE
   *
   * Using `Schedule#collect`, produce a schedule that recurs forever, collecting its
   * inputs into a list.
   */
  def collectedInputs[A]: Schedule[Any, A, Chunk[A]] =
    Schedule.collectAll

  /**
   * EXERCISE
   *
   * Using  `*>` (`zipRight`), combine `fiveTimes` and `everySecond` but return
   * the output of `everySecond`.
   */
  lazy val fiveTimesEverySecondR =
    fiveTimes *> everySecond

  /**
   * EXERCISE
   *
   * Produce a jittered schedule that first does exponential spacing (starting
   * from 10 milliseconds), but then after the spacing reaches 60 seconds,
   * switches over to fixed spacing of 60 seconds between recurrences, but will
   * only do that for up to 100 times, and produce a list of the inputs to
   * the schedule.
   */
  import zio.Random
  import Schedule.{ collectAll, exponential, fixed, recurs }
  def mySchedule[A]: Schedule[ZEnv, A, Chunk[A]] = {
    val exponential   = Schedule.exponential(10.millis)
    val spaced        = Schedule.spaced(50.seconds)
    val hundredTimes  = Schedule.recurs(100)
    val collectInputs = Schedule.collectAll[A]
    ((exponential || spaced) && hundredTimes).jittered *> collectInputs
  }
}
