package workshop

import zio._
import scala.collection.immutable.Nil
import scala.annotation.tailrec

object Looping extends ZIOAppDefault {
  import zio.Console._

  // Chunk
  // ZIO's general purpose collection type
  // Most similar to a Vector from the Scala standard library
  // Not as efficient as list for prepend or linear access
  // But much more efficient for random access, append, concatenation, etc...

  /**
   * EXERCISE
   *
   * Implement a `repeat` combinator using `flatMap` (or `zipRight`) and recursion.
   */
  def repeat[R, E, A](n: Int)(effect: ZIO[R, E, A]): ZIO[R, E, Chunk[A]] =
    if (n <= 0) ZIO.succeed(Chunk.empty)
    else effect.flatMap(a => repeat(n - 1)(effect).map(a +: _))

  def repeatDiscard[R, E, A](n: Int)(effect: ZIO[R, E, A]): ZIO[R, E, Unit] =
    if (n <= 0) ZIO.unit
    else (1 until n).foldLeft(effect)((acc, _) => acc *> effect).unit

  val run =
    repeatDiscard(1)(printLine("All work and no play makes Jack a dull boy"))
}

object Interview extends ZIOAppDefault {
  import java.io.IOException
  import zio.Console._

  val questions =
    "Where where you born?" ::
      "What color are your eyes?" ::
      "What is your favorite movie?" ::
      "What is your favorite number?" :: Nil

  /**
   * EXERCISE
   *
   * Implement the `getAllAnswers` function in such a fashion that it will ask
   * the user each question and collect them all into a list.
   */
  def getAllAnswers(questions: List[String]): ZIO[Console, IOException, List[String]] =
    questions match {
      case Nil     => ZIO.succeed(Nil)
      case q :: qs => getAnswer(q).flatMap(a => getAllAnswers(qs).map(a :: _))
    }

  def getAnswer(question: String): ZIO[Console, IOException, String] =
    for {
      _      <- Console.printLine(question)
      answer <- Console.readLine
    } yield answer

  /**
   * EXERCISE
   *
   * Use the preceding `getAllAnswers` function, together with the predefined
   * `questions`, to ask the user a bunch of questions, and print the answers.
   */
  val run =
    getAllAnswers(questions).debug("getAllAnswers")
}

object InterviewGeneric extends ZIOAppDefault {
  import java.io.IOException
  import zio.Console._

  val questions =
    "Where where you born?" ::
      "What color are your eyes?" ::
      "What is your favorite movie?" ::
      "What is your favorite number?" :: Nil

  /**
   * EXERCISE
   *
   * Implement the `iterateAndCollect` function.
   */
  def iterateAndCollect[R, E, A, B](as: List[A])(f: A => ZIO[R, E, B]): ZIO[R, E, List[B]] =
    as match {
      case Nil     => ZIO.succeed(Nil)
      case a :: as => f(a).flatMap(b => iterateAndCollect(as)(f).map(b :: _))
    }

  def getAnswer(question: String): ZIO[Console, IOException, String] =
    for {
      _      <- Console.printLine(question)
      answer <- Console.readLine
    } yield answer

  val run =
    iterateAndCollect(questions)(getAnswer).debug("iterateAndCollect")
}

object InterviewForeach extends ZIOAppDefault {
  import zio.Console._
  import java.io.IOException

  val questions =
    "Where where you born?" ::
      "What color are your eyes?" ::
      "What is your favorite movie?" ::
      "What is your favorite number?" :: Nil

  def getAnswer(question: String): ZIO[Console, IOException, String] =
    for {
      _      <- Console.printLine(question)
      answer <- Console.readLine
    } yield answer

  /**
   * EXERCISE
   *
   * Using `ZIO.foreach`, iterate over each question in `questions`, print the
   * question to the user (`printLine`), read the answer from the user
   * (`readLine`), and collect all answers into a collection. Finally, print
   * out the contents of the collection.
   */
  val run: ZIO[Console, IOException, List[String]] =
    ZIO.foreach(questions)(getAnswer).debug("foreach")

}

object WhileLoop extends ZIOAppDefault {
  import zio.Console._

  /**
   * EXERCISE
   *
   * Implement the functional effect version of a while loop.
   */
  def whileLoop[R, E, A](cond: ZIO[Any, Nothing, Boolean])(zio: ZIO[R, E, A]): ZIO[R, E, Chunk[A]] =
    cond.flatMap { b =>
      if (b) zio.flatMap(a => whileLoop(cond)(zio).map(a +: _))
      else ZIO.succeed(Chunk.empty)
    }

  val run = {
    def loop(variable: Ref[Int]) =
      whileLoop(variable.get.map(_ < 100)) {
        for {
          value <- variable.get
          _     <- printLine(s"At iteration: ${value}")
          _     <- variable.update(_ + 1)
        } yield ()
      }

    for {
      variable <- Ref.make(0)
      _        <- loop(variable)
    } yield 0
  }
}

object Iterate extends ZIOAppDefault {
  import zio.Console._

  /**
   * EXERCISE
   *
   * Implement the `iterate` function such that it iterates until the condition
   * evaluates to false, returning the "last" value of type `A`.
   */
  def iterate[R, E, A](start: A)(cond: A => Boolean)(f: A => ZIO[R, E, A]): ZIO[R, E, A] =
    ZIO.succeed(cond(start)).flatMap { b =>
      if (b) f(start).flatMap(a => iterate(a)(cond)(f))
      else ZIO.succeed(start)
    }

  val run =
    iterate(0)(_ < 100)(i => printLine(s"At iteration: ${i}").as(i + 1)).exitCode
}

object TailRecursive extends ZIOAppDefault {
  trait Response
  trait Request {
    def returnResponse(response: Response): Task[Unit]
  }

  lazy val acceptRequest: Task[Request] = Task(new Request {
    def returnResponse(response: Response): Task[Unit] =
      Task(println(s"Returning response ${response}"))
  })

  def handleRequest(request: Request): Task[Response] = Task {
    println(s"Handling request ${request}")
    new Response {}
  }

  /**
   * EXERCISE
   *
   * Make this infinite loop (which represents a webserver) effectfully tail
   * recursive.
   */
  lazy val webserver: ZIO[Any, Throwable, Nothing] =
    // acceptRequest.flatMap { request =>
    //   handleRequest(request).flatMap(request.returnResponse).flatMap(_ => webserver)
    // }
    (for {
      request  <- acceptRequest
      response <- handleRequest(request)
      _        <- request.returnResponse(response)
    } yield ()).forever

  val run =
    for {
      fiber <- webserver.fork
      _     <- ZIO.sleep(100.millis)
      _     <- fiber.interrupt
    } yield ()
}
