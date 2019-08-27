package matwojcik

import java.util.concurrent.Executors

import com.typesafe.scalalogging.StrictLogging
import scalaz.zio.DefaultRuntime
import scalaz.zio.ZIO

import scala.concurrent.ExecutionContext

object ZioApp extends scalaz.zio.App with StrictLogging {

  implicit val executionContext: ExecutionContext = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())
  implicit val runtime: scalaz.zio.Runtime[Environment] = new DefaultRuntime {}

  override def run(args: List[String]): ZIO[Environment, Nothing, Int] = ZIO.succeed(0)

}
