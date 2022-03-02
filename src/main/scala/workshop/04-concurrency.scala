package workshop

import zio._
import java.io.IOException

object FiberIntroduction extends ZIOAppDefault {

  // Fiber is the ZIO equivalent of a Thread
  // Fiber[+E, +A] is a running computation
  // Future[A] =?= Fiber[Throwable, A]
  // ZIO[-R, +E, +A] is a description of a workflow

  // Fiber
  //  1. Joining a fiber "semantically" blocks but does not actually block a thread
  //  2. Fibers have well defined semantics for interruption
  //  3. Fibers are very light weight

  // Thread
  //  1. Waiting for a result blocks the current thread
  //  2. Interrupting a thread is not very well defined
  //  3. Threads are heavy weight

  // Key operators on fibers
  //   1. fork - starts a new fiber that will run concurrently with the current one
  //   2. join - waits for the result of the fiber and makes it the result of the current one
  //   3. interrupt - interrupts the fiber

  val run =
    for {
      fiber <- ZIO.never.fork
      _     <- fiber.join
      _     <- Console.printLine("I'm done")
    } yield ()
}

object ForkJoin extends ZIOAppDefault {
  import zio.Console._

  val printer: ZIO[Console with Clock, IOException, Long] =
    printLine(".").repeat(Schedule.recurs(10))

  /**
   * EXERCISE
   *
   * Using `ZIO#fork`, fork the `printer` into a separate fiber, and then
   * print out a message, "Forked", then join the fiber using `Fiber#join`,
   * and finally, print out a message "Joined".
   */
  val run =
    for {
      fiber <- printer.fork
      _     <- Console.printLine("Forked")
      _     <- fiber.join
      _     <- Console.printLine("Joined")
    } yield ()

// Forked printing first was "accidental"
// Once we fork this is a concurrent process that could proceed faster or slower than main process

// Joined printing after the dots is "necessary"
// Join doesn't complete execution until the joined fiber completes execution

// Forked
// .
// .
// .
// .
// .
// .
// .
// .
// .
// .
// .
// Joined
}

object ForkInterrupt extends ZIOAppDefault {
  import zio.Console._

  val infinitePrinter =
    printLine(".").forever

  /**
   * EXERCISE
   *
   * Using `ZIO#fork`, fork the `printer` into a separate fiber, and then
   * print out a message, "Forked", then using `ZIO.sleep`, sleep for 100
   * milliseconds, then interrupt the fiber using `Fiber#interrupt`, and
   * finally, print out a message "Interrupted".
   */
  val run =
    for {
      fiber <- infinitePrinter.fork
      _     <- Console.printLine("Forked")
      _     <- ZIO.sleep(100.milliseconds)
      _     <- fiber.interrupt
      _     <- Console.printLine("Interrupted")
    } yield ()
}

object ParallelFib extends ZIOAppDefault {
  import zio.Console._

  // zip --> zipPar
  // foreach --> foreachPar

  /**
   * EXERCISE
   *
   * Rewrite this implementation to compute nth fibonacci number in parallel.
   */
  def fib(n: Int): UIO[BigInt] = {
    def loop(n: Int, original: Int): UIO[BigInt] =
      if (n <= 1) UIO(n)
      else
        UIO.suspendSucceed {
          for {
            left  <- loop(n - 1, original).fork
            right <- loop(n - 2, original).fork
            l     <- left.join
            r     <- right.join
          } yield l + r
        }

    loop(n, n)
  }

  val run =
    for {
      _ <- printLine(
            "What number of the fibonacci sequence should we calculate?"
          )
      n <- readLine.orDie.flatMap(input => ZIO(input.toInt)).eventually
      f <- fib(n)
      _ <- printLine(s"fib(${n}) = ${f}")
    } yield ()

  // 0, 1, 1, 2, 3, 5, 8
}

object AlarmAppImproved extends ZIOAppDefault {
  import zio.Console._
  import java.io.IOException
  import java.util.concurrent.TimeUnit

  lazy val getAlarmDuration: ZIO[Console, IOException, Duration] = {
    def parseDuration(input: String): IO[NumberFormatException, Duration] =
      ZIO
        .attempt(
          Duration((input.toDouble * 1000.0).toLong, TimeUnit.MILLISECONDS)
        )
        .refineToOrDie[NumberFormatException]

    val fallback = printLine("You didn't enter a number of seconds!") *> getAlarmDuration

    for {
      _        <- printLine("Please enter the number of seconds to sleep: ")
      input    <- readLine
      duration <- parseDuration(input) orElse fallback
    } yield duration
  }

  /**
   * EXERCISE
   *
   * Create a program that asks the user for a number of seconds to sleep,
   * sleeps the specified number of seconds using ZIO.sleep(d), concurrently
   * prints a dot every second that the alarm is sleeping for, and then
   * prints out a wakeup alarm message, like "Time to wakeup!!!".
   */
  val printer: ZIO[Console with Clock, IOException, Long] =
    Console.printLine(".") *> ZIO.sleep(1.second) *> printer
  val run =
    for {
      duration <- getAlarmDuration
      fiber    <- printer.fork
      _        <- ZIO.sleep(duration)
      _        <- fiber.interrupt
      _        <- Console.printLine("Time to wakeup!")
    } yield ()
}

/**
 * Effects can be forked to run in separate fibers. Sharing information between fibers can be done
 * using the `Ref` data type, which is like a concurrent version of a Scala `var`.
 */
object ComputePi extends ZIOAppDefault {
  import zio.Random._
  import zio.Console._
  import zio.Clock._
  import zio.stm._

  // Software transactional memory
  // Solution for composing atomic operations
  // "Transaction" description of changes we want to make to transactional variables
  // No variables changed yet
  // Expresses "intent" to change variables
  // "Commit" a transaction to actually update the variables
  // When we try to commit a transaction we check the journal and we check each of the variables to see if it was updated since we read from it
  // If it wasn't then we are safe to go ahead and commit the change
  // If it was then we throw away that transaction and try to compute it again
  // "Journal" is going to keep track of all the variables that we have read from while creating a transaction

  // STM version of any normal ZIO data structure
  // In zio.stm._ package

  // Trade off some performance for developer productivity

  /**
   * Some state to keep track of all points inside a circle,
   * and total number of points.
   */
  final case class PiState(
    inside: TRef[Long],
    total: TRef[Long]
  )

  object PiState {

    def make: ZIO[Any, Nothing, PiState] =
      for {
        inside <- TRef.make(0L).commit
        total  <- TRef.make(0L).commit
      } yield PiState(inside, total)
  }

  /**
   * A function to estimate pi.
   */
  def estimatePi(inside: Long, total: Long): Double =
    (inside.toDouble / total.toDouble) * 4.0

  /**
   * A helper function that determines if a point lies in
   * a circle of 1 radius.
   */
  def insideCircle(x: Double, y: Double): Boolean =
    Math.sqrt(x * x + y * y) <= 1.0

  /**
   * An effect that computes a random (x, y) point.
   */
  val randomPoint: ZIO[Random, Nothing, (Double, Double)] =
    nextDouble zip nextDouble

  def piWorker(piState: PiState): ZIO[Random, Nothing, Unit] =
    for {
      tuple  <- randomPoint
      (x, y) = tuple
      inside = insideCircle(x, y)
      _ <- (piState.inside.update(_ + 1).when(inside)
            *> piState.total.update(_ + 1)).commit
    } yield ()

  def piReporter(piState: PiState): ZIO[Console, IOException, Unit] =
    for {
      inside   <- piState.inside.get.commit
      total    <- piState.total.get.commit
      estimate = estimatePi(inside, total)
      _        <- Console.printLine(s"inside = $inside, total = $total")
      _        <- Console.printLine(estimate)
    } yield ()

  /**
   * EXERCISE
   *
   * Build a multi-fiber program that estimates the value of `pi`. Print out
   * ongoing estimates continuously until the estimation is complete.
   */
  val run =
    for {
      piState <- PiState.make
      fibers  <- ZIO.collectAll(Chunk.fill(8)(piWorker(piState).forever.fork))
      fiber   <- piReporter(piState).forever.fork
      _       <- Console.printLine("Press any key to exit...")
      _       <- Console.readLine
      _       <- ZIO.foreach(fibers)(_.interrupt)
      _       <- fiber.interrupt
    } yield ()
}

object ParallelZip extends ZIOAppDefault {
  import zio.Console._

  def fib(n: Int): UIO[Int] =
    if (n <= 1) UIO(n)
    else
      UIO.suspendSucceed {
        (fib(n - 1) zipWith fib(n - 2))(_ + _)
      }

  /**
   * EXERCISE
   *
   * Compute fib(10) and fib(13) in parallel using `ZIO#zipPar`, and display
   * the result.
   */
  val run =
    fib(10).zipPar(fib(13)).debug
}

object StmSwap extends ZIOAppDefault {
  import zio.Console._
  import zio.stm._

  /**
   * EXERCISE
   *
   * Demonstrate the following code does not reliably swap two values in the
   * presence of concurrency.
   */
  def exampleRef: UIO[Int] = {
    def swap[A](ref1: Ref[A], ref2: Ref[A]): UIO[Unit] =
      for {
        v1 <- ref1.get
        v2 <- ref2.get
        _  <- ref2.set(v1)
        _  <- ref1.set(v2)
      } yield ()

    for {
      ref1   <- Ref.make(100)
      ref2   <- Ref.make(0)
      fiber1 <- swap(ref1, ref2).repeatN(100).fork
      fiber2 <- swap(ref2, ref1).repeatN(100).fork
      _      <- (fiber1 zip fiber2).join
      value  <- (ref1.get zipWith ref2.get)(_ + _)
    } yield value
  }

  /**
   * EXERCISE
   *
   * Using `STM`, implement a safe version of the swap function.
   */
  def exampleStm: UIO[Int] = {
    def swap[A](ref1: TRef[A], ref2: TRef[A]): UIO[Unit] =
      (for {
        v1 <- ref1.get
        v2 <- ref2.get
        _  <- ref2.set(v1)
        _  <- ref1.set(v2)
      } yield ()).commit

    for {
      ref1   <- TRef.make(100).commit
      ref2   <- TRef.make(0).commit
      fiber1 <- swap(ref1, ref2).repeatN(100).fork
      fiber2 <- swap(ref2, ref1).repeatN(100).fork
      _      <- (fiber1 zip fiber2).join
      value  <- (ref1.get zipWith ref2.get)(_ + _).commit
    } yield value
  }

  val run =
    exampleStm.map(_.toString).flatMap(printLine(_))
}

object StmLock extends ZIOAppDefault {
  import zio.Console._
  import zio.stm._

  /**
   * EXERCISE
   *
   * Using STM, implement a simple binary lock by implementing the creation,
   * acquisition, and release methods.
   */
  class Lock private (tref: TRef[Boolean]) {
    def acquireSTM: ZSTM[Any, Nothing, Unit] =
      tref.get.flatMap { acquired =>
        if (acquired) STM.retry
        else tref.set(true)
      }
    def acquire: UIO[Unit] =
      acquireSTM.commit
    def releaseSTM: ZSTM[Any, Nothing, Unit] =
      tref.set(false)
    def release: UIO[Unit] =
      releaseSTM.commit
    def managed: ZManaged[Any, Nothing, Unit] =
      ZManaged.acquireRelease(acquire)(release)
    def withLock[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, A] =
      managed.use(_ => zio)
    def combine(that: Lock): Lock =
      ???
  }
  object Lock {
    def make: UIO[Lock] =
      for {
        tref <- TRef.make(false).commit
      } yield new Lock(tref)
  }

  // Actor(children: Set[Children], lock: Lock)
  // actor1
  // actor2

  // ZIO.succeed(println("Hello, World!")) // Okay // Great!
  // STM.succeed(println("Hello, World!")) // Big no! Don't do this!

  val run =
    (for {
      lock1 <- Lock.make
      lock2 <- Lock.make
      fiber1 <- ZIO
                 .acquireReleaseWith((lock1.acquireSTM *> lock2.acquireSTM).commit)(
                   _ => (lock1.releaseSTM *> lock2.releaseSTM).commit
                 )(_ => printLine("Bob  : I have both locks"))
                 .repeat(Schedule.recurs(10))
                 .fork
      fiber2 <- ZIO
                 .acquireReleaseWith((lock1.acquireSTM *> lock2.acquireSTM).commit)(
                   _ => (lock1.releaseSTM *> lock2.releaseSTM).commit
                 )(_ => printLine("Susan  : I have both locks"))
                 .repeat(Schedule.recurs(10))
                 .fork
      fiber3 <- lock1
                 .withLock(printLine("A troublemaker has one of the locks"))
                 .repeat(Schedule.recurs(10))
                 .fork
      fiber4 <- lock2
                 .withLock(printLine("Another troublemaker has one of the locks"))
                 .repeat(Schedule.recurs(10))
                 .fork
      _ <- (fiber1 zip fiber2).join
    } yield ()).exitCode
}

object StmQueue extends ZIOAppDefault {
  import zio.Console._
  import zio.stm._
  import scala.collection.immutable.{ Queue => ScalaQueue }

  /**
   * EXERCISE
   *
   * Using STM, implement a async queue with double back-pressuring.
   */
  class Queue[A] private (capacity: Int, ref: TRef[ScalaQueue[A]]) {
    def take: UIO[A] =
      ref.get.flatMap { queue =>
        queue.dequeueOption match {
          case Some((a, queue)) => ref.set(queue) *> ZSTM.succeed(a)
          case None             => STM.retry
        }
      }.commit
    def offer(a: A): UIO[Unit] =
      ref.get.flatMap { queue =>
        if (queue.length == capacity) STM.retry
        else ref.set(queue.enqueue(a))
      }.commit
  }
  object Queue {
    def bounded[A](capacity: Int): UIO[Queue[A]] =
      for {
        ref <- TRef.make(ScalaQueue.empty[A]).commit
      } yield new Queue(capacity, ref)
  }

  val run =
    for {
      queue <- Queue.bounded[Int](10)
      _     <- ZIO.foreach(0 to 100)(i => queue.offer(i).flatMap(_ => printLine(s"Offered: ${i}"))).fork
      _     <- ZIO.foreach(0 to 100)(_ => queue.take.flatMap(i => printLine(s"Took: ${i}")))
    } yield ()
}

object StmLunchTime extends ZIOAppDefault {
  import zio.Console._
  import zio.stm._

  /**
   * EXERCISE
   *
   * Using STM, implement the missing methods of Attendee.
   */
  final case class Attendee(state: TRef[Attendee.State]) {
    import Attendee.State._

    def isStarving: STM[Nothing, Boolean] = ???

    def feed: STM[Nothing, Unit] = ???
  }
  object Attendee {
    sealed trait State
    object State {
      case object Starving extends State
      case object Full     extends State
    }
  }

  /**
   * EXERCISE
   *
   * Using STM, implement the missing methods of Table.
   */
  final case class Table(seats: TArray[Boolean]) {
    def findEmptySeat: STM[Nothing, Option[Int]] =
      seats
        .fold[(Int, Option[Int])]((0, None)) {
          case ((index, z @ Some(_)), _) => (index + 1, z)
          case ((index, None), taken) =>
            (index + 1, if (taken) None else Some(index))
        }
        .map(_._2)

    def takeSeat(index: Int): STM[Nothing, Unit] = ???

    def vacateSeat(index: Int): STM[Nothing, Unit] = ???
  }

  /**
   * EXERCISE
   *
   * Using STM, implement a method that feeds a single attendee.
   */
  def feedAttendee(t: Table, a: Attendee): STM[Nothing, Unit] = ???

  /**
   * EXERCISE
   *
   * Using STM, implement a method that feeds only the starving attendees.
   */
  def feedStarving(table: Table, attendees: Iterable[Attendee]): UIO[Unit] =
    ???

  val run = {
    val Attendees = 100
    val TableSize = 5

    for {
      attendees <- ZIO.foreach(0 to Attendees)(
                    i =>
                      TRef
                        .make[Attendee.State](Attendee.State.Starving)
                        .map(Attendee(_))
                        .commit
                  )
      table <- TArray
                .fromIterable(List.fill(TableSize)(false))
                .map(Table(_))
                .commit
      _ <- feedStarving(table, attendees)
    } yield ExitCode.success
  }
}

object StmPriorityQueue extends ZIOAppDefault {
  import zio.Console._
  import zio.stm._

  /**
   * EXERCISE
   *
   * Using STM, design a priority queue, where smaller integers are assumed
   * to have higher priority than greater integers.
   */
  class PriorityQueue[A] private (
    minLevel: TRef[Option[Int]],
    map: TMap[Int, TQueue[A]]
  ) {
    def offer(a: A, priority: Int): STM[Nothing, Unit] = {
      val updateMinLevel = minLevel.get.flatMap {
        case Some(min) if priority < min => minLevel.set(Some(priority))
        case Some(min)                   => STM.unit
        case None                        => minLevel.set(Some(priority))
      }
      val updateMap = map.get(priority).flatMap {
        case Some(queue) => queue.offer(a).unit
        case None =>
          for {
            queue <- TQueue.unbounded[A]
            _     <- queue.offer(a)
            _     <- map.put(priority, queue)
          } yield ()
      }
      updateMinLevel *> updateMap
    }

    def take: STM[Nothing, A] =
      minLevel.get.flatMap {
        case Some(min) =>
          map.get(min).flatMap {
            case Some(queue) =>
              queue.take <*
                STM.whenSTM(queue.isEmpty) {
                  map.delete(min) *>
                    map.keys.flatMap { keys =>
                      if (keys.isEmpty) minLevel.set(None)
                      else minLevel.set(Some(keys.min))
                    }
                }
            case None => STM.retry
          }
        case None => STM.retry
      }
  }
  object PriorityQueue {
    def make[A]: STM[Nothing, PriorityQueue[A]] =
      for {
        minLevel <- TRef.make[Option[Int]](None)
        map      <- TMap.empty[Int, TQueue[A]]
      } yield new PriorityQueue(minLevel, map)
  }

  val run =
    for {
      _     <- printLine("Enter any key to exit...")
      queue <- PriorityQueue.make[String].commit
      lowPriority = ZIO.foreach(0 to 100) { i =>
        ZIO.sleep(1.millis) *> queue
          .offer(s"Offer: ${i} with priority 3", 3)
          .commit
      }
      highPriority = ZIO.foreach(0 to 100) { i =>
        ZIO.sleep(2.millis) *> queue
          .offer(s"Offer: ${i} with priority 0", 0)
          .commit
      }
      _ <- ZIO.forkAll(List(lowPriority, highPriority)) *> queue.take.commit
            .flatMap(printLine(_))
            .forever
            .fork *>
            readLine
    } yield 0
}

object StmReentrantLock extends ZIOAppDefault {
  import zio.Console._
  import zio.stm._

  private final case class WriteLock(
    writeCount: Int,
    readCount: Int,
    fiberId: FiberId
  )
  private final class ReadLock private (readers: Map[FiberId, Int]) {
    def total: Int = readers.values.sum

    def noOtherHolder(fiberId: FiberId): Boolean =
      readers.size == 0 || (readers.size == 1 && readers.contains(fiberId))

    def readLocks(fiberId: FiberId): Int =
      readers.get(fiberId).fold(0)(identity)

    def adjust(fiberId: FiberId, adjust: Int): ReadLock = {
      val total = readLocks(fiberId)

      val newTotal = total + adjust

      new ReadLock(
        readers =
          if (newTotal == 0) readers - fiberId
          else readers.updated(fiberId, newTotal)
      )
    }
  }
  private object ReadLock {
    val empty: ReadLock = new ReadLock(Map())

    def apply(fiberId: FiberId, count: Int): ReadLock =
      if (count <= 0) empty else new ReadLock(Map(fiberId -> count))
  }

  /**
   * EXERCISE
   *
   * Using STM, implement a reentrant read/write lock.
   */
  class ReentrantReadWriteLock(data: TRef[Either[ReadLock, WriteLock]]) {
    def writeLocks: UIO[Int] = ???

    def writeLocked: UIO[Boolean] = ???

    def readLocks: UIO[Int] = ???

    def readLocked: UIO[Boolean] = ???

    val read: Managed[Nothing, Int] = ???

    val write: Managed[Nothing, Int] = ???
  }
  object ReentrantReadWriteLock {
    def make: UIO[ReentrantReadWriteLock] = ???
  }

  val run = ???
}

object StmDiningPhilosophers extends ZIOAppDefault {
  import zio.Console._
  import zio.stm._
  import java.io.IOException

  sealed trait Fork
  val Fork = new Fork {}

  final case class Placement(
    left: TRef[Option[Fork]],
    right: TRef[Option[Fork]]
  )

  final case class Roundtable(seats: Vector[Placement])

  /**
   * EXERCISE
   *
   * Using STM, implement the logic of a philosopher to take not one fork, but
   * both forks when they are both available.
   */
  def takeForks(
    left: TRef[Option[Fork]],
    right: TRef[Option[Fork]]
  ): STM[Nothing, (Fork, Fork)] =
    for {
      leftFork <- left.get.flatMap {
                   case Some(fork) => left.set(None) *> STM.succeed(fork)
                   case None       => STM.retry
                 }
      rightFork <- right.get.flatMap {
                    case Some(fork) => right.set(None) *> STM.succeed(fork)
                    case None       => STM.retry
                  }
    } yield (leftFork, rightFork)

  /**
   * EXERCISE
   *
   * Using STM, implement the logic of a philosopher to release both forks.
   */
  def putForks(left: TRef[Option[Fork]], right: TRef[Option[Fork]])(
    tuple: (Fork, Fork)
  ): STM[Nothing, Unit] =
    for {
      _ <- left.get.flatMap {
            case Some(fork) => STM.retry
            case None       => left.set(Some(tuple._1))
          }
      _ <- right.get.flatMap {
            case Some(fork) => STM.retry
            case None       => right.set(Some(tuple._2))
          }
    } yield ()

  def setupTable(size: Int): ZIO[Any, Nothing, Roundtable] = {
    val makeFork = TRef.make[Option[Fork]](Some(Fork))

    (for {
      allForks0 <- STM.foreach(0 to size)(i => makeFork)
      allForks  = allForks0 ++ List(allForks0(0))
      placements = (allForks zip allForks.drop(1)).map {
        case (l, r) => Placement(l, r)
      }
    } yield Roundtable(placements.toVector)).commit
  }

  def eat(
    philosopher: Int,
    roundtable: Roundtable
  ): ZIO[Console, IOException, Unit] = {
    val placement = roundtable.seats(philosopher)

    val left  = placement.left
    val right = placement.right

    for {
      forks <- takeForks(left, right).commit
      _     <- printLine(s"Philosopher ${philosopher} eating...")
      _     <- putForks(left, right)(forks).commit
      _     <- printLine(s"Philosopher ${philosopher} is done eating")
    } yield ()
  }

  val run = {
    val count = 10

    def eaters(table: Roundtable): Iterable[ZIO[Console, IOException, Unit]] =
      (0 to count).map(index => eat(index, table))

    for {
      table <- setupTable(count)
      fiber <- ZIO.forkAll(eaters(table))
      _     <- fiber.join
      _     <- printLine("All philosophers have eaten!")
    } yield ()
  }
}
