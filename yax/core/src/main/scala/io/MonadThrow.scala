package io

#+cats
import cats.{Eval, Monad, Monoid}
import cats.data.{Kleisli, OptionT, StateT, WriterT}
#-cats

#+scalaz
import scalaz.{Kleisli, OptionT, Monad, Monoid, StateT, WriterT}
#-scalaz

trait MonadThrow[F[_]] extends Monad[F] {
  /** Throws an exception in the effect. */
  def throwM[A](e: Throwable): F[A]
}

object MonadThrow {
  def apply[F[_]](implicit F: MonadThrow[F]): MonadThrow[F] = F
}
