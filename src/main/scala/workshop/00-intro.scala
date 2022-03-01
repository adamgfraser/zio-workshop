package workshop

import zio._
import java.io.IOException

/*
 * INTRODUCTION
 *
 * ZIO effects are immutable data values that model a possibly complex series
 * of async, concurrent, resourceful, and contextual computations.
 *
 * The only effect type in ZIO is called ZIO, and has three type parameters,
 * which permit accessing context from an environment (`R`), failing with a
 * value of a certain type (`E`), and succeeding with a value of a certain
 * type (`A`).
 *
 * Unlike Scala's Future, ZIO effects are completely lazy. All methods on ZIO
 * effects return new ZIO effects. No part of the workflow is executed until
 * one of the `unsafeRun*` functions are called.
 *
 * ZIO effects are transformed and combined using methods on the ZIO data type.
 * For example, two effects can be combined into a sequential workflow using
 * an operator called `zip`. Similarly, two effects can be combined into a
 * parallel workflow using an operator called `zipPar`.
 *
 * The operators on the ZIO data type allow very powerful, expressive, and
 * type-safe transformation and composition, while the methods in the ZIO
 * companion object allow building new effects from simple values (which are
 * not themselves effects).
 *
 * In this section, you will explore both the ZIO data model itself, as well
 * as the very basic operators used to transform and combine ZIO effects, as
 * well as a few simple ways to build effects.
 */

object EitherExample extends scala.App {

  def Try[A](a: => A): Either[Throwable, A] =
    try {
      Right(a)
    } catch {
      case e: Throwable => Left(e)
    }

  val myEither = Try(throw new NoSuchElementException("foo"))

  println(s"The result of myEither is $myEither")
}

object ZIOWorkflow extends ZIOAppDefault {

  // ZIO is a workflow

  // trait ZIO[-R, +E, +A]

  // Future[A]

  // ZIO[Any, Nothing, Chunk[DatabaseResult]]

  // Logging
  // Database
  // Authentication

  val run: ZIO[Console, IOException, Unit] =
    Console.printLine("Hello, world!")
}

object ZIOModel {

  /**
   * EXERCISE
   *
   * Implement all missing methods on the ZIO companion object.
   */
  object ZIO {
    def succeed[A](a: => A): ZIO[Any, Nothing, A] =
      ZIO(_ => Right(a))

    def fail[E](e: => E): ZIO[Any, E, Nothing] =
      ZIO(_ => Left(e))

    import scala.util.Try
    import scala.util.Success
    import scala.util.Failure

    def attempt[A](sideEffect: => A): ZIO[Any, Throwable, A] =
      ZIO { _ =>
        Try(sideEffect) match {
          case Success(e) => Right(e)
          case Failure(f) => Left(f)
        }
      }

    def environment[R]: ZIO[R, Nothing, R] =
      ZIO(r => Right(r))

    def access[R, A](f: R => A): ZIO[R, Nothing, A] =
      environment.map(f)

    def accessZIO[R, E, A](f: R => ZIO[R, E, A]): ZIO[R, E, A] =
      environment.flatMap(f)
  }

  /**
   * EXERCISE
   *
   * Implement all missing methods on the ZIO class.
   */
  final case class ZIO[-R, +E, +A](run: R => Either[E, A]) { self =>
    def map[B](f: A => B): ZIO[R, E, B] =
      flatMap(a => ZIO.succeed(f(a)))

    def flatMap[R1 <: R, E1 >: E, B](f: A => ZIO[R1, E1, B]): ZIO[R1, E1, B] =
      ZIO { r =>
        self.run(r) match {
          case Left(e)  => Left(e)
          case Right(a) => f(a).run(r)
        }
      }

    def zip[R1 <: R, E1 >: E, B](that: ZIO[R1, E1, B]): ZIO[R1, E1, (A, B)] =
      self.flatMap(a => that.map(b => (a, b)))

    def either: ZIO[R, Nothing, Either[E, A]] =
      ZIO(r => Right(self.run(r)))

    def provide(r: R): ZIO[Any, E, A] =
      ZIO(_ => self.run(r))

    def orDie(implicit ev: E <:< Throwable): ZIO[R, Nothing, A] =
      ZIO(r => self.run(r).fold(throw _, Right(_)))
  }

  def printLine(line: String): ZIO[Any, Nothing, Unit] =
    ZIO.attempt(println(line)).orDie

  val readLine: ZIO[Any, Nothing, String] =
    ZIO.attempt(scala.io.StdIn.readLine()).orDie

  def unsafeRun[A](zio: ZIO[Any, Throwable, A]): A =
    zio.run(()) match {
      case Left(t)  => throw t
      case Right(a) => a
    }

  /**
   * Run the following main function and compare the results with your
   * expectations.
   */
  def main(args: Array[String]): Unit =
    unsafeRun {
      printLine("Hello, what is your name?").flatMap(_ => readLine.flatMap(name => printLine(s"Your name is: ${name}")))
    }
}

object ZIOTypes {
  type ??? = Nothing

  // trait ZIO[-R, +E, +A]
  // Task[A]

  /**
   * EXERCISE
   *
   * Provide definitions for the ZIO type aliases below.
   */
  type Task[+A]     = ZIO[Any, Throwable, A]
  type UIO[+A]      = ZIO[Any, Nothing, A]
  type RIO[-R, +A]  = ZIO[R, Throwable, A]
  type IO[+E, +A]   = ZIO[Any, E, A]
  type URIO[-R, +A] = ZIO[R, Nothing, A]

  // Unexceptional
}

object HelloWorld extends ZIOAppDefault {
  import Console.printLine

  /**
   * EXERCISE
   *
   * Implement a simple "Hello World!" program using the effect returned by
   * `printLine`.
   */
  val run =
    Console.printLine("Hello ZIO Workshop!")
}

object SimpleMap extends ZIOAppDefault {
  import Console.readLine

  /**
   * EXERCISE
   *
   * Using `ZIO#map`, map the string success value of `readLine` into an
   * integer (the length of the string).
   */
  val run: ZIO[Console, IOException, Int] =
    Console.readLine.map(_.length).debug("length")
}

object PrintSequenceZip extends ZIOAppDefault {
  import Console.printLine

  /**
   * EXERCISE
   *
   * Using `zip`, compose a sequence of `printLine` effects to produce an effect
   * that prints three lines of text to the console.
   */
  val run =
    Console.printLine("hello").zip(Console.printLine("world")).zip(Console.printLine("!"))
}

object PrintSequence extends ZIOAppDefault {
  import Console.printLine

  /**
   * EXERCISE
   *
   * Using `*>` (`zipRight`), compose a sequence of `printLine` effects to
   * produce an effect that prints three lines of text to the console.
   */
  val run =
    Console.printLine("hello") *>
      Console.printLine("world") *>
      Console.printLine("!")
}

object PrintReadSequence extends ZIOAppDefault {
  import Console.{ printLine, readLine }

  /**
   * EXERCISE
   *
   * Using `*>` (`zipRight`), sequentially compose a `printLine` effect, which
   * models printing out "Hit Enter to exit...", together with a `readLine`
   * effect, which models reading a line of text from the console.
   */
  val run =
    Console.printLine("Hit enter to exit...") *>
      Console.readLine
}

object SimpleDuplication extends ZIOAppDefault {
  import Console.printLine

  /**
   * EXERCISE
   *
   * In the following program, the expression `printLine("Hello again")`
   * appears three times. Factor out this duplication by introducing a new
   * value that stores the expression, and then referencing that variable
   * three times.
   */
  val run =
    printLine("What's your name?") *>
      printLine("Hello again") *>
      printLine("Hello again") *>
      printLine("Hello again")
}

object FlatMap extends ZIOAppDefault {
  import Console.{ printLine, readLine }

  /**
   * EXERCISE
   *
   * The following program is intended to ask the user for their name, then
   * read their name, then print their name back out to the user. However,
   * the `zipRight` (`*>`) operator is not powerful enough to solve this
   * problem, because it does not allow a _subsequent_ effect to depend
   * on the success value produced by a _preceding_ effect.
   *
   * Solve this problem by using the `ZIO#flatMap` operator, which composes
   * a first effect together with a "callback", which can return a second
   * effect that depends on the success value produced by the first effect.
   */
  val run =
    for {
      _    <- Console.printLine("What is your name?")
      name <- Console.readLine
      _    <- Console.printLine(s"Hello, ${name}!")
    } yield ()
}

object PromptName extends ZIOAppDefault {
  import Console.{ printLine, readLine }

  /**
   * EXERCISE
   *
   * The following program uses a combination of `zipRight` (`*>`), and
   * `flatMap`. However, this makes the structure of the program harder
   * to understand. Replace all `zipRight` by `flatMap`, by ignoring the
   * success value of the left hand effect.
   */
  val run = {
    printLine("What is your name?") *>
      readLine.flatMap(name => printLine(s"Your name is: ${name}"))
  }.exitCode

  /**
   * EXERCISE
   *
   * Implement a generic "zipRight" that sequentially composes the two effects
   * using `flatMap`, but which succeeds with the success value of the effect
   * on the right-hand side.
   */
  def myZipRight[R, E, A, B](
    left: ZIO[R, E, A],
    right: ZIO[R, E, B]
  ): ZIO[R, E, B] =
    ???
}

object ForComprehension extends ZIOAppDefault {
  import Console.{ printLine, readLine }

  /**
   * EXERCISE
   *
   * Rewrite the following program to use a `for` comprehension.
   */
  val run =
    printLine("What is your name?")
      .flatMap(_ => readLine.flatMap(name => printLine(s"Your name is: ${name}")))
      .exitCode
}

object ForComprehensionBackward extends ZIOAppDefault {
  import Console.{ printLine, readLine }

  val readInt = readLine.flatMap(string => ZIO(string.toInt)).orDie

  /**
   * EXERCISE
   *
   * Rewrite the following program, which uses a `for` comprehension, to use
   * explicit `flatMap` and `map` methods. Note: each line of the `for`
   * comprehension will translate to a `flatMap`, except the final line,
   * which will translate to a `map`.
   */
  val run = {
    for {
      _   <- printLine("How old are you?")
      age <- readInt
      _ <- if (age < 18) printLine("You are a kid!")
          else printLine("You are all grown up!")
    } yield ()
  }.exitCode
}

object NumberGuesser extends ZIOAppDefault {
  import Console.{ printLine, readLine }
  import Random._

  def analyzeAnswer(random: Int, guess: String): ZIO[Console, IOException, Unit] =
    if (random.toString == guess.trim) printLine("You guessed correctly!")
    else printLine(s"You did not guess correctly. The answer was ${random}")

  // Console
  // Random
  // System
  // Clock

  /**
   * EXERCISE
   *
   * Choose a random number (using `nextInt`), and then ask the user to guess
   * the number (using `readLine`), feeding their response to `analyzeAnswer`,
   * above.
   */
  val run =
    for {
      random <- Random.nextInt
      _      <- Console.printLine("Guess a number!")
      guess  <- Console.readLine
      _      <- analyzeAnswer(random, guess)
    } yield ()
}

object SingleSideEffect extends ZIOAppDefault {

  /**
   * EXERCISE
   *
   * Using ZIO.attempt, convert the side-effecting of `println` into a pure
   * functional effect.
   */
  def myPrintLn(line: String): ZIO[Any, Throwable, Unit] =
    ZIO.attempt(println(line))

  val run =
    myPrintLn("Hello World!")
}

object MultipleSideEffects extends ZIOAppDefault {

  /**
   * Using `ZIO.attempt`, wrap Scala's `println` method to lazily convert it
   * into a functional effect, which describes the action of printing a line
   * of text to the console, but which does not actually perform the print.
   */
  def printLine(line: String): Task[Unit] =
    ZIO.attempt(println(line))

  /**
   * Using `ZIO.attempt`, wrap Scala's `scala.io.StdIn.readLine()` method to
   * lazily convert it into a functional effect, which describes the action
   * of printing a line of text to the console, but which does not actually
   * perform the print.
   */
  val readLine: Task[String] =
    ZIO.attempt(scala.io.StdIn.readLine)

  val run = {
    for {
      _    <- printLine("Hello, what is your name?")
      name <- readLine
      _    <- printLine(s"Good to meet you, ${name}!")
    } yield ()
  }.exitCode
}
