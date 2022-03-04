package workshop

import zio._
import zio.stream._

object Streams {

  // ZStream[-R, +E, +A]
  // ZIO requires an R and either fails with an E or produces exactly one A
  // ZStream requires R produces 0 or more A values or fails with an E

  // ZIO[Any, Nothing, Int]
  ZIO.succeed(1)
  ZIO.succeed(42)

  // ZStream[Any, Nothing, Int]
  ZStream.empty
  ZStream(1)
  ZStream(1, 2, 3)

  // ZIO produces its value "all at once"
  // ZStream produces its value "incrementally"

  def doSomething: ZIO[Any, Nothing, Int] =
    ???

  for {
    fiber <- doSomething.fork
    _     <- fiber.join
  } yield ()

  // Stream as an "effectual iterator"

  trait Iterator[+A] {
    def hasNext(): Boolean
    def next(): A
  }

  object Iterator {

    type ??? = Nothing

    def make(): Iterator[???] =
      ???
  }

  // Failing with Some(E) is failing with an error
  // Failing with None is end of stream
  // Succeeding with Chunk(elements) here are some elements and you can try to pull again to see if there are more in the future

  object ExampleImplementation {

    final case class ZStream[-R, +E, +A](pull: ZManaged[R, E, ZIO[R, Option[E], Chunk[A]]]) {

      def map[B](f: A => B): ZStream[R, E, B] =
        ZStream(pull.map(_.map(_.map(f))))
    }

    val zIterator: ZStream[Any, Nothing, Int] = ???

    zIterator.pull.use { pull =>
      for {
        first  <- pull
        second <- pull
        third  <- pull
      } yield (first, second, third)
    }
  }
}

object RealStreams extends ZIOAppDefault {

  val stream = Stream(1, 2, 3)

  // [info] Chunk(1,2,3)
  // (Chunk(1),Chunk(2),Chunk(3))

  val run =
    ???

  List(1, 2, 3).sorted

  // map
  // flatMap
  // filter
  // collect
  // zip
}

object StreamFiles extends ZIOAppDefault {
  import java.io.IOException
  import scala.io.Source
  import CatAcquireRelease.readFile

  val cool =
    "src/main/scala/workshop/cool.txt"

  def openSource(file: String): ZIO[Any, IOException, Source] =
    ZIO.attemptBlockingIO(Source.fromFile(file)) <* ZIO.debug("opening source")

  def closeSource(source: Source): ZIO[Any, Nothing, Unit] =
    ZIO.attemptBlockingIO(source.close()).orDie *> ZIO.debug("closing source")

  def managedSource(file: String): ZManaged[Any, IOException, Source] =
    ZManaged.acquireReleaseWith(openSource(file))(closeSource)

  def readFileStream(file: String): ZStream[Any, IOException, String] =
    ZStream.managed(managedSource(file)).flatMap { source =>
      ZStream.fromIterator(source.getLines).refineToOrDie[IOException]
    }

  // ZStream.async { emit =>
  //   ???
  // }

// [info] opening source
// [info] line: foo
// [info] line: fee
// [info] line: fi
// [info] closing source

  val run =
    readFileStream(cool)
      .flatMap(line => ZStream.fromIterable(line.split(" ")))
      .filter(_.startsWith(("f")))
      .take(3)
      .foreach(line => Console.printLine(s"line: $line"))
}

object ConcurrentStreams extends ZIOAppDefault {

  val fastProducer: ZStream[Clock, Nothing, String] =
    ZStream.iterate(0)(_ + 1).map(n => s"fast: $n").tap(_ => ZIO.sleep(100.milliseconds))

  val slowProducer: ZStream[Clock, Nothing, String] =
    ZStream.iterate(0)(_ + 1).map(n => s"slow: $n").tap(_ => ZIO.sleep(1000.milliseconds))

  val zippedStream =
    fastProducer.zip(slowProducer)

  val zipWithLatestStream =
    fastProducer.zipWithLatest(slowProducer)((_, _))

  val mergedStream =
    fastProducer.merge(slowProducer)

  def runStream[R, E, A](stream: ZStream[R, E, A]): ZIO[R, E, Unit] =
    stream.tap(a => ZIO.debug(a)).take(20).runDrain

  val run =
    runStream(mergedStream)
}

object StreamArchitecture {
  import java.io.IOException

  // ZStream takes no inputs and produces some outputs

  // ZIO[R, E, A]

  // Inputs - Could be from some kind of external API (Twitter, polling, etc...)

  // Transformations - Our logic

  // Outputs - Write to a database, fire off notifications to our clients, log this somewhere, trigger our own trading events

  // Stream(inputs) >>> Pipeline(stream operators) >>> Sink(outputs)
  // Inputs >>> TransformationA >>> TransformationB >>> TransformationC >>> Outputs
  // TransformationA (X -> Y)
  // TransformationB (Y -> Z)

  // Inputs                                 TransformationC (Y, Z => W)   Outputs
  //          TransformationA (X -> Y)
  //          TransformationB (X -> Z)

  val input: ZStream[Any, Nothing, Int] =
    ZStream(1, 2, 3)

  val output: ZSink[Console, IOException, Any, Nothing, Unit] =
    ZSink.foreach[Console, IOException, Any](Console.printLine(_))

  val transformationA: ZPipeline[Any, Nothing, Int, Int] =
    ZPipeline.map[Int, Int](_ + 1)

  val transformationB: ZPipeline[Any, Nothing, Int, Int] =
    ZPipeline.map[Int, Int](_ * 2)

  val transformationC: ZPipeline[Any, Nothing, Int, String] =
    ZPipeline.map[Int, String](_.toString)

  // Channel
  // ZChannel[-Env, -InErr, -InElem, -InDone, +OutErr, +OutElem, +OutDone]

  // Streams    ---        Pipelines       ---       Sinks

  // Stream doesn't take any inputs and emits zero or more values
  // Pipeline takes into values and also emits values
  // Sink takes in values and emits at most one summary value

  val run =
    input >>> transformationA >>> transformationB >>> transformationC >>> output
    // input                   // Inputs
    //   .via(transformationA) // TransformationA
    //   .via(transformationB) // TransformationB
    //   .via(transformationC) // TransformationC
    //   .run(output)          // Outputs

  def transformationA[R, E](stream: ZStream[R, E, Int]): ZStream[R, E, Int] =
    stream.map(_ + 1)
}
