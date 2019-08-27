package matwojcik

import cats.data.AndThen
import cats.Applicative
import cats.Monad
import cats.StackSafeMonad
import cats.effect.Sync
import cats.syntax.all._
import cats.instances.all._
import cats.effect.concurrent.Ref
import cats.mtl.DefaultMonadState
import cats.mtl.MonadState
import cats.mtl.instances.all._
import cats.mtl.syntax.all._

class RefMonadState[F[_]: Sync, S, A](val runF: F[Ref[F, S] => F[(Ref[F, S], A)]]) {

  def runWith(initial: S): F[(S, A)] =
    for {
      ref    <- Ref[F].of(initial)
      result <- runWithRef(ref)
      s      <- result._1.get
    } yield (s, result._2)

  def runWithRef(initial: Ref[F, S]): F[(Ref[F, S], A)] =
    for {
      f      <- runF
      result <- f(initial)
    } yield result

}

object RefMonadState {

  def apply[F[_]: Sync, S, A](f: Ref[F, S] => F[(Ref[F, S], A)]) = new RefMonadState[F, S, A](Sync[F].pure(f))

  implicit def monadState[F[_]: Sync, S]: MonadState[RefMonadState[F, S, *], S] = new DefaultMonadState[RefMonadState[F, S, *], S] {
    override val monad: Monad[RefMonadState[F, S, *]] = monadForRefMonadState

    override def get: RefMonadState[F, S, S] = RefMonadState(s => s.get.map((s, _)))

    override def set(s: S): RefMonadState[F, S, Unit] = RefMonadState(st => st.set(s).map((st, _)))
  }

  implicit def monadForRefMonadState[F[_]: Sync, S]: Monad[RefMonadState[F, S, *]] = new StackSafeMonad[RefMonadState[F, S, *]] {
    override def flatMap[A, B](fa: RefMonadState[F, S, A])(f: A => RefMonadState[F, S, B]): RefMonadState[F, S, B] =
      new RefMonadState[F, S, B](
        fa.runF
          .map(
            rf =>
              AndThen(rf).andThen(
                fff =>
                  fff.flatMap {
                    case (s, a) =>
                      f(a).runWithRef(s)
                  }
              )
          )
      )

    override def pure[A](x: A): RefMonadState[F, S, A] = RefMonadState(s => Applicative[F].pure((s, x)))
  }

}
