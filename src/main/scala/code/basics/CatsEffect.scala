package code.basics

import cats.effect.{ExitCode, IOApp, Resource}

import scala.io.StdIn


object CatsEffect {

  // IO

  import cats.effect.IO

  val io1: IO[Int] = IO.pure(11) // eager
  val io2 = IO.apply { // lazy
    11
  }

  def evaluateIO[A](io: IO[A]): A = {
    // importing all global objects to handle software infra functionalities like thread handling
    import cats.effect.unsafe.implicits.global
    io.unsafeRunSync()
  }

  val smallProgram: IO[Unit] = for {
    line1 <- IO(StdIn.readLine())
    line2 <- IO(StdIn.readLine())
    _ <- IO(println(s"${line1} - ${line2}"))
  } yield ()


  def main(args: Array[String]): Unit = {
    val result = evaluateIO(io2)
    println(s"result of lazy evaluation ${result}")
    evaluateIO(smallProgram)
  }

}


object CatsEffectApp extends IOApp.Simple {

  import cats.effect.IO
  import scala.concurrent.duration.*

  val smallProgram: IO[Unit] = for {
    line1 <- IO(StdIn.readLine())
    line2 <- IO(StdIn.readLine())
    _ <- IO(println(s"${line1} - ${line2}"))
  } yield ()


  // Error Handling

  val aFailure = IO.raiseError(new RuntimeException("Something is wrong!"))
  aFailure.handleErrorWith {
    case _: RuntimeException => IO(println("error handled."))
  }

  // Fibers

  // *> = flatMap
  val delayedPrint = IO.sleep(1.second) *> IO(println("delayed print..."))

  val multiFiberProgram = for {
    fiber1 <- delayedPrint.start
    fiber2 <- delayedPrint.start
    _ <- fiber1.join()
    _ <- fiber2.join()
  } yield ()

  val cancelingFiber = for {
    fiber <- delayedPrint.onCancel("Im canceled!").start
    _ <- IO.sleep(500.millis) *> IO(println("lets cancel fiber")) *> fiber.cancel
    _ <- fiber.join()
  } yield ()


  // Resources

  val fileResource = Resource.make(
    IO(scala.io.Source.fromFile("src/main/resources/data.json"))
  ) (source => IO(println("closing source")) *> IO(source.close()))


  override def run = for {
    _ <- delayedPrint
    _ <- smallProgram
  } yield ()


}