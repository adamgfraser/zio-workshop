package workshop

import zio._
import zio.test._
import zio.test.Assertion._
import zio.test.TestAspect._
import java.io.IOException

object WorkshopSpec extends ZIOSpecDefault {
  import TicTacToe._

  val greeter: ZIO[Console, IOException, Unit] =
    for {
      _    <- Console.printLine("What's your name?")
      name <- Console.readLine
      _    <- Console.printLine(s"Hello, $name")
    } yield ()

  def spec = suite("some suite")(
    test("some test") {
      for {
        _      <- TestConsole.feedLines("Adam")
        _      <- greeter
        output <- TestConsole.output
      } yield assertTrue(
        output(0) == "What's your name?\n" &&
          output(1) == "Hello, Adam\n"
      )
    } @@ silent,
    test("integer addition is associative") {
      check(Gen.int, Gen.int, Gen.int) { (x, y, z) =>
        val left = (x + y) + z
        val right = x + (y + z)
        assertTrue(left == right)  
      }
    }
  )
}
