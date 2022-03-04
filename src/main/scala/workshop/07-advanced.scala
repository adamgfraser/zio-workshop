package workshop

import zio._
import scala.concurrent.ExecutionContext

object PoolLocking extends ZIOAppDefault {
  import zio.Console._

  lazy val dbPool: Executor = Executor.fromExecutionContext(1024)(ExecutionContext.global)

  /**
   * EXERCISE
   *
   * Using `ZIO#onExecutor`, write an `onDatabase` combinator that runs the
   * specified effect on the database thread pool.
   */
  def onDatabase[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, A] =
    zio.onExecutor(dbPool)

  /**
   * EXERCISE
   *
   * Implement a combinator to print out thread information before and after
   * executing the specified effect.
   */
  def threadLogged[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, A] = {
    val log = ZIO.succeed {
      val thread = Thread.currentThread()

      val id        = thread.getId()
      val name      = thread.getName()
      val groupName = thread.getThreadGroup().getName()

      println(s"Thread($id, $name, $groupName)")
    }

    log *> zio <* log
  }

  /**
   * EXERCISE
   *
   * Use the `threadLogged` combinator around different effects below to
   * determine which threads are executing which effects.
   */
  val run =
    threadLogged(printLine("Main")) *>
      onDatabase {
        threadLogged(printLine("Database")) *>
          ZIO.blocking {
            threadLogged(printLine("Blocking"))
          } *>
          threadLogged(printLine("Database"))
      } *>
      threadLogged(printLine("Main"))
}

object PlatformTweaking extends scala.App {
  import Console._
  import zio.internal.Platform

  /**
   * EXERCISE
   *
   * Modify the default platform by specifying a custom behavior for logging errors.
   */
  lazy val platform = RuntimeConfig.default.copy(loggers = ???)

  val environment = Runtime.default.environment

  val myWorkflow =
    ZIO.logInfo("Hello from ZIO!")

  val logger: ZLogger[String, Unit] =
    new ZLogger[String, Unit] {
      def apply(
        trace: ZTraceElement,
        fiberId: FiberId,
        logLevel: LogLevel,
        message: () => String,
        context: Map[FiberRef.Runtime[_], AnyRef],
        spans: List[LogSpan],
        location: ZTraceElement,
        annotations: Map[String, String]
      ): Unit =
        println(s"$logLevel: ${message()}")
    }

  /**
   * EXERCISE
   *
   * Create a custom runtime using `platform` and `environment`, and use this to
   * run an effect.
   */
  lazy val customRuntime: Runtime[ZEnv] = Runtime.default.mapRuntimeConfig { defaultRuntimeConfig =>
    defaultRuntimeConfig.copy(loggers = defaultRuntimeConfig.loggers.add(logger))
  }
  val exampleRun = customRuntime.unsafeRun(myWorkflow)

// [info] timestamp=2022-03-02T18:49:13.328670Z level=INFO thread=#zio-fiber-0 message="Hello from ZIO!" location=workshop.PlatformTweaking.myWorkflow file=07-advanced.scala line=72
// [info] LogLevel(20000,INFO,6): Hello from ZIO!
}

object Sharding extends ZIOAppDefault {
  import zio.Console._

  /**
   * EXERCISE
   *
   * Create N workers reading from a Queue, if one of them fails, then wait
   * for the other ones to process their current item, but terminate all the
   * workers.
   *
   * Return the first error, or never return, if there is no error.
   */
  def shard[R, E, A](
    queue: Queue[A],
    n: Int,
    worker: A => ZIO[R, E, Unit]
  ): ZIO[R, Nothing, E] = {

    def shardWorker(ref: Ref[Option[E]]): ZIO[R, Nothing, Unit] =
      ref.get.flatMap {
        case Some(_) =>
          ZIO.unit
        case None =>
          queue.take
            .flatMap(worker)
            .foldZIO(
              e =>
                ref.update {
                  case None    => Some(e)
                  case Some(e) => Some(e)
                },
              _ => shardWorker(ref)
            )

      }

    for {
      ref     <- Ref.make[Option[E]](None)
      workers <- ZIO.collectAll(Chunk.fill(n)(shardWorker(ref).fork))
      _       <- ZIO.foreach(workers)(_.join)
      error   <- ref.get
    } yield error.get
  }

  val run = {
    def makeWorker(ref: Ref[Int]): Int => ZIO[Console, String, Unit] =
      (work: Int) =>
        for {
          count <- ref.get
          _ <- if (count < 100) printLine(s"Worker is processing item ${work} after ${count}").orDie
              else ZIO.fail(s"Uh oh, failed processing ${work} after ${count}")
          _ <- ref.update(_ + 1)
        } yield ()

    for {
      queue <- Queue.bounded[Int](100)
      ref   <- Ref.make(0)
      _     <- queue.offer(1).forever.fork
      error <- shard(queue, 10, makeWorker(ref))
      _     <- printLine(s"Failed with ${error}")
    } yield ()
  }
}
