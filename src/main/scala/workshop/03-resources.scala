package workshop

import zio._
import java.text.NumberFormat
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import java.io.IOException

object Cat extends ZIOAppDefault {
  import zio.Console._
  import java.io.IOException

  /**
   * EXERCISE
   *
   * Using `ZIO.attemptBlocking`, implement a function to read a file on the
   * blocking thread pool, storing the result into a string.
   */
  def readFile(file: String): ZIO[Any, IOException, String] =
    ZIO
      .attemptBlocking(scala.io.Source.fromFile(file).getLines.mkString("\n"))
      .refineToOrDie[IOException]

  /**
   * EXERCISE
   *
   * Implement a version of the command-line utility "cat", which dumps the
   * contents of the specified file to standard output.
   */
  val run =
    for {
      args   <- ZIO.succeed("src/main/scala/workshop/cool.txt")
      string <- readFile(args)
      _      <- Console.printLine(string)
    } yield ()
}

object CatEnsuring extends ZIOAppDefault {
  import zio.Console._
  import java.io.IOException
  import scala.io.Source

  def open(file: String): ZIO[Any, IOException, Source] =
    ZIO.attemptBlockingIO(scala.io.Source.fromFile(file))

  def close(source: Source): ZIO[Any, IOException, Unit] =
    ZIO.attemptBlockingIO(source.close())

  // Guarantee of ensuring is that if the effect BEGINS execution then the
  // finalizer will be run

  /**
   * EXERCISE
   *
   * Using `ZIO#ensuring`, implement a safe version of `readFile` that cannot
   * fail to close the file, no matter what happens during reading.
   */
  def readFile(file: String): ZIO[Any, IOException, String] =
    ZIO.uninterruptible {
      for {
        source   <- open(file)
        contents <- ZIO.attempt(source.getLines().mkString("\n")).ensuring(close(source).orDie)
      } yield contents
    }.refineToOrDie[IOException]

  val run =
    for {
      args <- ZIO.succeed(Chunk("src/main/scala/workshop/cool.txt"))
      fileName <- ZIO
                   .fromOption(args.headOption)
                   .tapError(_ => printLine("You must specify a file name on the command line"))
      contents <- readFile(fileName)
      _        <- printLine(contents)
    } yield ()
}

object CatAcquireRelease extends ZIOAppDefault {
  import zio.Console._
  import java.io.IOException
  import scala.io.Source

  def open(file: String): ZIO[Any, IOException, Source] =
    ZIO.attemptBlockingIO(scala.io.Source.fromFile(file))

  def close(source: Source): ZIO[Any, IOException, Unit] =
    ZIO.attemptBlockingIO(source.close())

  // Guarantee of acquireRelease is that if `acquire` action successfully
  // completes execution then `release` action will be run no matter what

  /**
   * EXERCISE
   *
   * Using `ZIO#acquireRelease`, implement a safe version of `readFile` that
   * cannot fail to close the file, no matter what happens during reading.
   */
  def readFile(file: String): ZIO[Any, IOException, String] =
    ZIO.acquireReleaseWith(open(file))(close(_).orDie) { source =>
      ZIO.attemptBlockingIO(source.getLines().mkString("\n"))
    }

  val run = {
    for {
      args <- ZIO.succeed(Chunk("src/main/scala/workshop/cool.txt"))
      fileName <- ZIO
                   .fromOption(args.headOption)
                   .tapError(_ => printLine("You must specify a file name on the command line"))
      contents <- readFile(fileName)
      _        <- printLine(contents)
    } yield ()
  }.exitCode
}

object SourceManaged extends ZIOAppDefault {
  import zio.Console._
  import java.io.IOException

  import scala.io.Source

  final class ZSource private (private val source: Source) {
    def execute[T](f: Source => T): ZIO[Any, IOException, T] =
      ZIO.attemptBlockingIO(f(source))
  }
  object ZSource {

    /**
     * EXERCISE
     *
     * Use the `ZManaged.make` constructor to make a managed data type that
     * will automatically acquire and release the resource when it is used.
     */
    def make(file: String): ZManaged[Any, IOException, ZSource] = {
      // An effect that acquires the resource:
      val open = ZIO.attemptBlockingIO(new ZSource(Source.fromFile(file)))

      // A function that, when given the resource, returns an effect that
      // releases the resource:
      val close: ZSource => ZIO[Any, Nothing, Unit] =
        _.execute(_.close()).orDie <* ZIO.debug(s"Closing $file")

      ZManaged.acquireReleaseWith(open)(close)
    }
  }

  val managed = ZSource.make("cool.txt")
  // a description of a workflow that requires an environment R, can fail with
  // an error E, if successful produces a value A which requires some finalization
  // trait ZManaged[-R, +E, +A]
  // map
  // flatMap
  // zip
  // foreach
  // zipPar
  // foreachPar
  // orElse
  // catchAll

  /**
   * EXERCISE
   *
   * Using `ZManaged.foreachPar` and other functions as necessary, implement a function
   * to read the contents of all files in parallel, but ensuring that if anything
   * goes wrong during parallel reading, all files are safely closed.
   */
  def readFiles(
    files: List[String]
  ): ZIO[Console, IOException, List[String]] =
    ZManaged.foreachPar(files)(ZSource.make).use { sources =>
      ZIO.foreachPar(sources)(_.execute(_.getLines().mkString("\n")))
    }

  /**
   * EXERCISE
   *
   * Implement a function that prints out all files specified on the
   * command-line. Only print out contents from these files if they
   * can all be opened simultaneously. Otherwise, don't print out
   * anything except an error message.
   */
  val run =
    for {
      file1    <- ZIO.succeed("src/main/scala/workshop/cool.txt")
      file2    <- ZIO.succeed("src/main/scala/workshop/howdy.txt")
      contents <- readFiles(List(file1, file2))
      _        <- ZIO.foreach(contents)(Console.printLine(_))
    } yield ()
}

object CatIncremental extends ZIOAppDefault {
  import zio.Console._
  import java.io.{ FileInputStream, IOException, InputStream }

  final case class FileHandle private (private val is: InputStream) {
    final def close: ZIO[Any, IOException, Unit] = ZIO.attemptBlockingIO(is.close()) *> ZIO.debug(s"Closing $is")

    final def read: ZIO[Any, IOException, Option[Chunk[Byte]]] =
      ZIO.attemptBlockingIO {
        val array = Array.ofDim[Byte](1024)
        val len   = is.read(array)
        if (len < 0) None
        else Some(Chunk.fromArray(array).take(len))
      }
  }

  /**
   * EXERCISE
   *
   * Refactor `FileHandle` so that creating it returns a `ZManaged`, so that
   * it is impossible to forget to close an open handle.
   */
  object FileHandle {
    final def open(file: String): ZManaged[Any, IOException, FileHandle] = {
      val acquire = ZIO.attemptBlockingIO(new FileHandle(new FileInputStream(file)))
      ZManaged.acquireReleaseWith(acquire)(_.close.orDie)
    }
  }

  /**
   * EXERCISE
   *
   * Implement an incremental version of `cat` that pulls a chunk of bytes at
   * a time, stopping when there are no more chunks left.
   */
  def cat(fh: FileHandle): ZIO[Console, IOException, Unit] =
    fh.read.flatMap {
      case Some(chunk) => Console.printLine(chunk) *> cat(fh)
      case None        => ZIO.unit
    }

  /**
   * EXERCISE
   *
   * Implement an incremental version of the `cat` utility, using
   * `ZIO#acquireRelease` or `ZManaged` to ensure the file is closed in the
   * event of error or interruption.
   */
  val run =
    ZIO.succeed(Chunk("src/main/scala/workshop/cool.txt")).flatMap { args =>
      if (args.isEmpty) printLine("Usage: cat <file>")
      else
        /**
         * EXERCISE
         *
         * Open the specified file, safely create and use a file handle to
         * incrementally dump the contents of the file to standard output.
         */
        FileHandle.open(args(0)).use(cat)
    }
}

object Challenge extends ZIOAppDefault {

  // Open one file, read all of its contents, and write those contents to a
  // second file.

  // First, do this with `ZIO.acquireReleaseWith`

  // Second, do it with `ZManaged`

  import scala.io.Source

  def openSource(name: String): ZIO[Any, IOException, Source] =
    ZIO.attemptBlockingIO(Source.fromFile(name))

  def closeSource(source: Source): ZIO[Any, Nothing, Unit] =
    ZIO.attemptBlockingIO(source.close()).orDie *> ZIO.debug("closing source")

  def managedSource(name: String): ZManaged[Any, IOException, Source] =
    ZManaged.acquireReleaseWith(openSource(name))(closeSource)

  import java.io.BufferedWriter
  import java.io.FileWriter

  def openWriter(name: String): ZIO[Any, IOException, BufferedWriter] =
    ZIO.attemptBlockingIO(new BufferedWriter(new FileWriter(name)))

  def closeWriter(writer: BufferedWriter): ZIO[Any, Nothing, Unit] =
    ZIO.attemptBlockingIO(writer.close()).orDie *> ZIO.debug("closing writer")

  def managedWriter(name: String): ZManaged[Any, IOException, BufferedWriter] =
    ZManaged.acquireReleaseWith(openWriter(name))(closeWriter)

  def managedSourceAndWriter(from: String, to: String): ZManaged[Any, IOException, (Source, BufferedWriter)] =
    managedSource(from) zip managedWriter(to)

  // Acquire A
  // Acquire B
  // Release B
  // Release A

  val from = "src/main/scala/workshop/cool.txt"
  val to   = "src/main/scala/workshop/howdy.txt"

  val run =
    managedSourceAndWriter(from, to).use {
      case (source, writer) =>
        ZIO.attemptBlockingIO(source.getLines().foreach(writer.write(_))) *>
          ZIO.attemptBlockingIO(writer.flush())
    }
}
