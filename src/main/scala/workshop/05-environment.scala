package workshop

import zio._

object AccessEnvironment extends ZIOAppDefault {
  import zio.Console._

  // trait ZIO[R, E, A]

  // 1. How do we "access" the environment?
  // 2. How do we define our services and layers that build those services?
  // 3. How do we provide those layers to our applications

  final case class Config(server: String, port: Int)

  /**
   * EXERCISE
   *
   * Using `ZIO.serviceWith`, access a `Config` type from the environment, and
   * extract the `server` field from it.
   */
  val accessServer: ZIO[Config, Nothing, String] =
    ZIO.serviceWith[Config](_.server)

  /**
   * EXERCISE
   *
   * Using `ZIO.serviceWith`, access a `Config` type from the environment, and
   * extract the `port` field from it.
   */
  val accessPort: ZIO[Config, Nothing, Int] =
    ZIO.serviceWith[Config](_.port)

  // "Recipe" for building a set of services
  // ZLayer[-RIn, -E, +ROut]
  // Often we need to perform effects or do finalization as part of building our dependencies
  // Often one set of services will depend on another set of services
  // Database service depends on the configuration and logging

  val run = {
    val config = Config("localhost", 7878)

    val configLayer =
      ZLayer.succeed(config)

    (for {
      server <- accessServer
      port   <- accessPort
      _      <- UIO(println(s"Configuration: ${server}:${port}"))
    } yield ()).provide(configLayer)
  }
}

object ProvideEnvironment extends ZIOAppDefault {
  import zio.Console._

  final case class Config(server: String, port: Int)

  final case class DatabaseConnection() {
    def query(query: String): ZIO[Any, Throwable, Int] = Task(42)
  }

  val getServer: ZIO[Config, Nothing, String] =
    ZIO.serviceWith[Config](_.server)

  val useDatabaseConnection: ZIO[DatabaseConnection, Throwable, Int] =
    ZIO.serviceWithZIO[DatabaseConnection](_.query("SELECT * FROM USERS"))

  /**
   * EXERCISE
   *
   * Compose both the `getServer` and `useDatabaseConnection` effects together.
   * In order to do this successfully, you will have to use `ZIO#provide` to
   * give them the environment that they need in order to run, then they can
   * be composed because their environment types will be compatible.
   */
  val run = {
    val config = Config("localhost", 7878)

    val configLayer: ZLayer[Any, Nothing, Config] =
      ZLayer.succeed(config)

    val databaseLayer: ZLayer[Any, Nothing, DatabaseConnection] =
      ZLayer.succeed(DatabaseConnection())

    val myZIO: ZIO[Config with DatabaseConnection, Throwable, Unit] =
      for {
        server <- getServer
        _      <- useDatabaseConnection
        _      <- ZIO.debug("It's alive!")
      } yield ()

    myZIO.provide(configLayer, databaseLayer)
  }
}

object CakeEnvironment extends ZIOAppDefault {
  import zio.Console._
  import java.io.IOException

  type MyFx = Logging with Files

  trait Logging {
    val logging: Logging.Service
  }
  object Logging {
    trait Service {
      def log(line: String): UIO[Unit]
    }
    def log(line: String) = ZIO.serviceWithZIO[Logging](_.logging.log(line))
  }
  trait Files {
    val files: Files.Service
  }
  object Files {
    trait Service {
      def read(file: String): IO[IOException, String]
    }
    def read(file: String) = ZIO.serviceWithZIO[Files](_.files.read(file))
  }

  def build(logging0: Logging, files0: Files): Logging with Files =
    new Logging with Files {
      val logging: Logging.Service = logging0.logging
      val files: Files.Service     = files0.files
    }

  val effect =
    for {
      file <- Files.read("build.sbt")
      _    <- Logging.log(file)
    } yield ()

  /**
   * EXERCISE
   *
   * Run `effect` by using `ZIO#provide` to give it what it needs. You will
   * have to build a value (the environment) of the required type
   * (`Files with Logging`).
   */
  val run =
    effect
      .provideLayer(???)
      .exitCode
}

/**
 * Although there are no requirements on how the ZIO environment may be used to pass context
 * around in an application, for easier composition, ZIO includes a data type called `Environment`,
 * which represents a map from a type to an object that satisfies that type. Sometimes, this is
 * called a "ZEnvironment Map" or more precisely, a "type-indexed map".
 */
object ZEnvironmentMap extends ZIOAppDefault {
  trait Logging
  object Logging extends Logging

  trait Database
  object Database extends Database

  trait Cache
  object Cache extends Cache

  // ZEnvironment == type level map
  // Map from types of services to implementations of those services

  // ZEnvironment[Logging with Files]
  // Map(
  //   Logging.typeTag -> someParticularLoggingService,
  //   Files.typeTag   -> someParticularFilesService
  // )

  // ZEnvironment[Logging]
  // Map(
  //   Logging.typeTag -> someParticularLoggingService
  // )

  // ZEnvironment[Files]
  // Map(
  //   Files.typeTag -> someParticularFilesService
  // )

  final case class SimpleZIO[-R, +E, +A](run: R => Either[E, A])
  final case class SimpleZIO1[-R, +E, +A](run: R => Either[Cause[E], A])
  final case class SimpleZIO2[-R, +E, +A](run: ZEnvironment[R] => Either[Cause[E], A])

  val hasLogging = ZEnvironment(Logging: Logging)

  val hasDatabase = ZEnvironment(Database: Database)

  val hasCache = ZEnvironment(Cache: Cache)

  /**
   * EXERCISE
   *
   * Using the `++` operator on `ZEnvironment`, combine the three maps (`hasLogging`, `hasDatabase`, and
   * `environmentCache`) into a single map that has all three objects.
   */
  val environment: ZEnvironment[Database with Logging with Cache] =
    hasLogging ++ hasDatabase ++ hasCache

  /**
   * EXERCISE
   *
   * Using `ZEnvironment#get`, which can retrieve an object stored in the map, retrieve the logging,
   * database, and cache objects from `allThree`. Note that you will have to specify the type
   * parameter, as it cannot be inferred (the map needs to know which of the objects you want to
   * retrieve, and that can be specified only by type).
   */
  lazy val logging  = environment.get[Logging]
  lazy val database = environment.get[Database]
  lazy val cache    = environment.get[Cache]

  // ZIO.service
  // ZIO.serviceWith
  // ZIO.serviceWithZIO

  // ZIO.environment
  // ZIO.environmentWith
  // ZIO.environmentWithZIO

  val run =
    (for {
      environment <- ZIO.environment[Database with Logging with Cache]
      logging     = environment.get[Logging]
      database    = environment.get[Database]
      cache       = environment.get[Cache]
      _           <- ZIO.debug(s"Logging: $logging")
      _           <- ZIO.debug(s"Database: $database")
      _           <- ZIO.debug(s"Cache: $cache")
    } yield ()).provideEnvironment(environment)
}

/**
 * In ZIO, layers are essentially wrappers around constructors for services in your application.
 * Services provide functionality like persistence or logging or authentication, and they are used
 * by business logic.
 *
 * A layer is a lot like a constructor, except that a constructor can only "construct" a single
 * service. Layers can construct many services. In addition, this construction can be resourceful,
 * and even asynchronous or concurrent.
 *
 * Layers bring more power and compositionality to constructors. Although you don't have to use
 * them to benefit from ZIO environment, they can make it easier to assemble applications out of
 * modules without having to do any wiring, and with great support for testability.
 *
 * ZIO services like `Clock` and `System` are all designed to work well with layers.
 */
object LayerEnvironment extends ZIOAppDefault {
  import zio.Console._
  import java.io.IOException

  type MyFx = Logging with Files

  trait Files {
    def read(file: String): ZIO[Any, IOException, String]
  }

  object Files extends Accessible[Files] {
    import scala.io.Source

    /**
     * EXERCISE
     *
     * Using `ZLayer.succeed`, create a layer that implements the `Files`
     * service.
     */
    val live: ZLayer[Any, Nothing, Files] =
      ZLayer.succeed {
        new Files {
          def open(file: String): ZIO[Any, IOException, Source] =
            ZIO.attemptBlockingIO(scala.io.Source.fromFile(file))
          def close(source: Source): ZIO[Any, IOException, Unit] =
            ZIO.attemptBlockingIO(source.close())
          def read(file: String): ZIO[Any, IOException, String] =
            ZIO.acquireReleaseWith(open(file))(close(_).orDie) { source =>
              ZIO.attemptBlockingIO(source.getLines().mkString("\n"))
            }
        }
      }

    def read(file: String): ZIO[Files, IOException, String] =
      ZIO.serviceWithZIO[Files](_.read(file))
  }

  // val noAccessor = for {
  //   contents <- Files.read("cool.txt")
  //   _        <- Console.printLine(contents)
  // } yield ()

  // object ComplexExample {

  //   object persistence {
  //     trait Persistence {
  //       def save(string: String): ZIO[Any, IOException, Unit]
  //     }
  //   }

  //   object cassandraPersistence {
  //     import persistence._
  //     val cassandraLayer: ZLayer[Any, Nothing, Persistence] =
  //       ???
  //   }

  //   object postgresPersistence {
  //     import persistence._
  //     val postgresLayer: ZLayer[Any, Nothing, Persistence] =
  //       ???
  //   }

  //   // Depend on persistence BUT NOT cassandraPersistence or postgresPersistence
  //   object businessLogic {}
  // }

  trait Logging {
    def log(line: String): ZIO[Any, Nothing, Unit]
  }
  object Logging extends Accessible[Logging] {

    final case class ConsoleLogging(console: Console, clock: Clock) extends Logging {
      import java.util.concurrent.TimeUnit
      def log(line: String): ZIO[Any, Nothing, Unit] =
        for {
          time <- clock.currentTime(TimeUnit.MILLISECONDS)
          _    <- console.printLine(s"$time: $line").orDie
        } yield ()
    }

    /**
     * EXERCISE
     *
     * Using a `for` comprehension, create a layer that requires `Console`
     * and uses the console to provide a logging service.
     */
    val live: ZLayer[Clock with Console, Nothing, Logging] =
      ZLayer {
        for {
          _       <- ZIO.debug("Logging layer created").toManaged
          console <- ZManaged.service[Console]
          clock   <- ZManaged.service[Clock]
          _       <- ZManaged.finalizer(ZIO.debug("Logging layer finalized"))
        } yield ConsoleLogging(console, clock)
      }

    def log(line: String) = Logging(_.log(line))
  }

  /**
   * EXERCISE
   *
   * Discover the inferred type of `effect`, and write it out explicitly.
   */
  val effect =
    for {
      file <- Files.read("build.sbt")
      _    <- Logging.log(file)
    } yield ()

  val run = {

    /**
     * EXERCISE
     *
     * Create a layer using `ZLayer.make` and specifying all the pieces that go into the layer.
     */
    val fullLayer: ZLayer[Any, Nothing, Files with Logging] =
      ZLayer.make[Files with Logging](Files.live, Logging.live, Clock.live, Console.live)

    /**
     * EXERCISE
     *
     * Using `ZIO#provide`, inject the full layer into the effect to remove its dependencies.
     */
    val effect2: ZIO[Any, IOException, Unit] =
      effect.provide(fullLayer)

    /**
     * EXERCISE
     *
     * Run `effect` by using `ZIO#provide` to give it what it needs. You will have
     * to give it a list of services that implement its required dependencies.
     */
    effect2
  }

  // Different "layers"

  // Service that handles user notifications based on various business rules
  // Inner most layer should be closest to our business logic and should be almost entirely business logic
  // Inner layer depends on one or more "middle layers" that are farther from the business logic
  // Middle layers depend on one or more "outer layers" that are even farther from business logic

  trait Schedule
  type Message = String

  trait UserNotifications {
    def send(user: String, message: Message, schedule: Schedule): ZIO[Any, Nothing, Unit]
  }

  object UserNotifications {

    final case class UserNotificationsLive(sms: SMSService, email: EmailService) extends UserNotifications {
      def send(user: String, message: Message, schedule: Schedule): ZIO[Any, Nothing, Unit] =
        ???
    }
  }

  trait SMSService {
    def sendSMS(user: String, message: Message): ZIO[Any, Nothing, Unit]
  }

  object SMSService {
    final case class SMSServiceLive(http: HttpService) extends SMSService {
      def sendSMS(user: String, message: Message): ZIO[Any, Nothing, Unit] =
        ZIO.debug(s"Sending SMS to $user: $message").as(())
    }
  }

  trait EmailService {
    def sendEmail(user: String, message: Message): ZIO[Any, Nothing, Unit]
  }

  object EmailService {
    final case class EmailServiceLive(http: HttpService) extends EmailService {
      def sendEmail(user: String, message: Message): ZIO[Any, Nothing, Unit] =
        ZIO.debug(s"Sending email to $user: $message").as(())
    }
  }

  trait HttpService {}

  object HttpService {
    final case class HttpServiceLive() extends HttpService
  }
}
