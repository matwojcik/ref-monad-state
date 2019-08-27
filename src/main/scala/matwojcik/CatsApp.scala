package matwojcik

import cats.Monad
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.Sync
import cats.mtl.MonadState
import cats.syntax.all._
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger

object CatsApp extends IOApp {

  private val logger = Slf4jLogger.getLogger[IO]

  override def run(args: List[String]): IO[ExitCode] =
    program[RefMonadState[IO, List[String], *]]
      .runWith(Nil)
      .flatMap {
        case (s, res) =>
          logger.info(s"Result: $res, state: $s")
      }
      .as(ExitCode.Success)

  def program[F[_]: Monad](implicit MS: MonadState[F, List[String]]): F[Int] =
    for {
      result <- 1.pure[F]
      _      <- MS.modify(_ :+ "Elem1")
      _      <- MS.modify(_ :+ "Elem2")
//      _      <- MS.set(List("Elem3"))
    } yield result
}
