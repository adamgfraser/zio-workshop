package workshop

import zio._
import java.io.IOException

/*
 * INTRODUCTION
 *
 * ZIO effects model failure, in a way similar to the Scala data types `Try`
 * and `Either`. Unlike exceptions, ZIO effects are statically-typed, allowing
 * you to determine if and how effects fail by looking at type signatures.
 *
 * ZIO effects have a large number of error-related operators to transform
 * and combine effects. Some of these "catch" errors, while others transform
 * them, and still others combine potentially failing effects with fallback
 * effects.
 *
 * In this section, you will learn about all these operators, as well as the
 * rich underlying model of errors that ZIO uses internally.
 */

object ErrorConstructor extends ZIOAppDefault {
  import zio.Console._

  /**
   * EXERCISE
   *
   * Using `ZIO.fail`, construct an effect that models failure with any
   * string value, such as "Uh oh!". Explain the type signature of the
   * effect.
   */
  val failed: ZIO[Any, String, Nothing] =
    ZIO.fail("Uh oh!")

  val run =
    failed.foldZIO(printLine(_), printLine(_))
}

object ErrorRecoveryOrElse extends ZIOAppDefault {
  import zio.Console._

  val failed = ZIO.fail("Uh oh!")

  val succeeded: ZIO[Any, Throwable, Int] =
    ZIO.succeed(13)

  /**
   * EXERCISE
   *
   * Using `ZIO#orElse` have the `run` function compose the preceding `failed`
   * effect with another effect that succeeds with a success exit code.
   */
  val run =
    (succeeded orElse ZIO.succeed(42)).debug("ErrorRecoveryOrElse")
}

object ErrorShortCircuit extends ZIOAppDefault {
  import zio.Console._

  val failed =
    printLine("About to fail...") *>
      ZIO.fail("Uh oh!").orElse(ZIO.succeed("I'm okay")) *>
      printLine("This will NEVER be printed!")

  /**
   * EXERCISE
   *
   * Using `ZIO#orElse`, compose the `failed` effect with another effect that
   * succeeds.
   */
  val run =
    failed
}

object ErrorRecoveryFold extends ZIOAppDefault {
  import zio.Console._

  val failed = ZIO.fail("Uh oh!")

  // def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] =
  //   ???

  /**
   * EXERCISE
   *
   * Using `ZIO#fold`, transform both failure and success values.
   */
  val run: ZIO[Any, Nothing, String] =
    failed
      .fold(
        s => s"fold recovered from original failure $s",
        _ => "this should never happen"
      )
      .debug("ErrorRecoveryFold")

  val failedEither =
    Left("Uh oh!")

  val recoveredEither = failedEither.fold(
    s => s"fold recovered from original failure $s",
    _ => "this should never happen"
  )

  println(recoveredEither)
}

object ErrorRecoveryCatchAll extends ZIOAppDefault {
  import zio.Console._

  val failed = ZIO.fail("Uh oh!")

  /**
   * EXERCISE
   *
   * Using `ZIO#catchAll`, catch all errors in `failed` and print them out to
   * the console using `printLine`.
   */
  val run =
    failed.catchAll(e => Console.printLineError(s"Caught error: $e") *> ZIO.fail(e))

  // try {
  //   ???
  // } catch {
  //   case e =>
  //     println(e)
  //     throw e
  // }
}

object ErrorRecoveryFoldZIO extends ZIOAppDefault {
  import zio.Console._

  val failed: ZIO[Any, String, String] = ZIO.fail("Uh oh!")

  /**
   * EXERCISE
   *
   * Using `ZIO#foldZIO`, print out the success or failure value of `failed`
   * by using `printLine`.
   */
  val run =
    failed.foldCauseZIO(
      cause => Console.printLine(cause),
      a => Console.printLine(a)
    )

  val zio1 = ZIO.succeed(1)
  val zio2 = ZIO.succeed(2)
}

object ErrorRecoveryEither extends ZIOAppDefault {
  import zio.Console._

  val failed: ZIO[Any, String, Int] = ZIO.fail("Uh oh!")

  /**
   * EXERCISE
   *
   * Using `ZIO#either`, surface the error of `failed` into the success
   * channel, and then map the `Either[String, Int]`.
   */
  val run: ZIO[Any, Nothing, Either[String, Int]] =
    failed.either.debug("Error recovery either")
}

object ErrorRecoveryIgnore extends ZIOAppDefault {
  import zio.Console._

  val failed: ZIO[Any, String, Int] = ZIO.fail("Uh oh!")

  /**
   * EXERCISE
   *
   * Using `ZIO#ignore`, simply ignore the failure of `failed`.
   */
  val run =
    failed.ignore.debug("ignore example")
}

object ErrorNarrowing extends ZIOAppDefault {
  import java.io.IOException
  import scala.io.StdIn.readLine

  val broadReadLine: IO[Throwable, String] = ZIO.attempt(scala.io.StdIn.readLine())

  /**
   * EXERCISE
   *
   * Using `ZIO#refineToOrDie`, narrow the error type of `broadReadLine` into
   * an `IOException`:
   */
  val myReadLine: IO[IOException, String] = ???

  def myPrintLn(line: String): UIO[Unit] = UIO(println(line))

  val run =
    for {
      _    <- myPrintLn("What is your name?")
      name <- myReadLine
      _    <- myPrintLn(s"Good to meet you, ${name}")
    } yield ()
}

object AlarmApp extends ZIOAppDefault {
  import zio.Console._
  import java.io.IOException
  import java.util.concurrent.TimeUnit

  /**
   * EXERCISE
   *
   * Create an effect that will get a `Duration` from the user, by prompting
   * the user to enter a decimal number of seconds. Use `refineToOrDie` to
   * narrow the error type as necessary.
   */
  lazy val getAlarmDuration: ZIO[Console, IOException, Duration] = {
    def parseDuration(input: String): ZIO[Any, NumberFormatException, Duration] =
      ZIO
        .attempt(Duration.fromMillis((input.toDouble * 1000).toLong))
        .refineToOrDie[NumberFormatException]

    def fallback(input: String): ZIO[Console, IOException, Duration] =
      for {
        _        <- Console.printLine(s"$input was invalid!")
        duration <- getAlarmDuration
      } yield duration

    for {
      _        <- printLine("Please enter the number of seconds to sleep: ")
      input    <- readLine
      duration <- parseDuration(input) orElse fallback(input)
    } yield duration
  }

  /**
   * EXERCISE
   *
   * Create a program that asks the user for a number of seconds to sleep,
   * sleeps the specified number of seconds using `ZIO.sleep(d)`, and then
   * prints out a wakeup alarm message, like "Time to wakeup!!!".
   */
  val run =
    for {
      duration <- getAlarmDuration
      _        <- ZIO.sleep(duration)
      _        <- Console.printLine("Time to wakeup!")
    } yield ()
}

object SequentialCause extends ZIOAppDefault {
  import zio.Console._

  // R => Either[E, A]
  // R => Either[Cause[E], A]

  // def fail[E](e: E): ZIO[Any, E, A] =
  //   ZIO(_ => Left(Cause.fail(e))

  val example: ZIO[Any, Nothing, Nothing] =
    ZIO.succeed(throw new NoSuchElementException)

  object Example {

    sealed trait Cause[+E]

    object Cause {
      final case class Fail[E](e: E)                            extends Cause[E]
      final case class Die(t: Throwable)                        extends Cause[Nothing]
      final case class Then[E](left: Cause[E], right: Cause[E]) extends Cause[E]
      final case class Both[E](left: Cause[E], right: Cause[E]) extends Cause[E]
    }
  }

  val failed1 = Cause.fail("Uh oh 1")
  val failed2 = Cause.fail("Uh oh 2")

  /**
   * EXERCISE
   *
   * Using `Cause.++`, form a sequential cause by composing `failed1`
   * and `failed2`.
   */
  lazy val composed =
    failed1 ++ failed2

  /**
   * EXERCISE
   *
   * Using `Cause.prettyPrint`, dump out `composed` to the console.
   */
  val run =
    Console.printLine(composed.prettyPrint)
}

object ParalellCause extends ZIOAppDefault {
  import zio.Console._

  val failed1 = Cause.fail("Uh oh 1")
  val failed2 = Cause.fail("Uh oh 2")

  /**
   * EXERCISE
   *
   * Using `Cause.&&`, form a parallel cause by composing `failed1`
   * and `failed2`.
   */
  lazy val composed =
    failed1 && failed2

  /**
   * EXERCISE
   *
   * Using `Cause.prettyPrint`, dump out `composed` to the console.
   */
  val run =
    Console.printLine(composed.prettyPrint)
}

object Sandbox extends ZIOAppDefault {
  import zio.Console._

  val failed1    = ZIO.fail("Uh oh 1")
  val failed2    = ZIO.fail("Uh oh 2")
  val finalizer1 = ZIO.fail(new Exception("Finalizing 1!")).orDie
  val finalizer2 = ZIO.fail(new Exception("Finalizing 2!")).orDie

  val composed = ZIO.uninterruptible {
    (failed1 ensuring finalizer1) zipPar (failed2 ensuring finalizer2)
  }

  /**
   * EXERCISE
   *
   * Using `ZIO#sandbox`, sandbox the `composed` effect and print out the
   * resulting `Cause` value to the console using `printLine`.
   */
  val run =
    composed.sandbox.catchAll { cause =>
      Console.printLine(cause.prettyPrint) *>
        ZIO.fail("Failed")
    }
}
