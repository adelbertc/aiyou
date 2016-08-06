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

object MonadThrow extends MonadThrowInstances {
  def apply[F[_]](implicit F: MonadThrow[F]): MonadThrow[F] = F
}

private[io] sealed trait MonadThrowInstances {
  implicit def ioMonadThrowForKleisli[F[_]: MonadThrow, X]: MonadThrow[Kleisli[F, X, ?]] =
    new MonadThrowInstance[Kleisli[?[_], X, ?], F] {
      val monad =
#+cats
        Kleisli.kleisliMonadReader[F, X]
#-cats

#+scalaz
        Kleisli.kleisliMonadReader[F, X]
#-scalaz

      def throwM[A](e: Throwable): Kleisli[F, X, A] =
        Kleisli(_ => MonadThrow[F].throwM(e))
    }

  implicit def ioMonadThrowForOptionT[F[_]: MonadThrow]: MonadThrow[OptionT[F, ?]] =
    new MonadThrowInstance[OptionT, F] {
      val monad =
#+cats
        OptionT.optionTMonad[F]
#-cats

#+scalaz
        OptionT.optionTMonadPlus[F]
#-scalaz

      def throwM[A](e: Throwable): OptionT[F, A] = OptionT(MonadThrow[F].throwM(e))
    }

  implicit def ioMonadThrowForStateT[F[_]: MonadThrow, X]: MonadThrow[StateT[F, X, ?]] =
    new MonadThrowInstance[StateT[?[_], X, ?], F] {
      val monad =
#+cats
        StateT.stateTMonadState[F, X]
#-cats

#+scalaz
        StateT.stateTMonadState[X, F]
#-scalaz

      def throwM[A](e: Throwable): StateT[F, X, A] = StateT(_ => MonadThrow[F].throwM(e))
    }

  implicit def ioMonadThrowForWriterT[F[_]: MonadThrow, X: Monoid]: MonadThrow[WriterT[F, X, ?]] =
    new MonadThrowInstance[WriterT[?[_], X, ?], F] {
      val monad =
#+cats
        WriterT.writerTMonadWriter[F, X]
#-cats

#+scalaz
        WriterT.writerTMonadListen[F, X]
#-scalaz

      def throwM[A](e: Throwable): WriterT[F, X, A] =WriterT(MonadThrow[F].throwM(e))
    }
}

private[io] trait MonadThrowInstance[FT[_[_], _], F[_]] extends MonadThrow[FT[F, ?]] {
  def monad: Monad[FT[F, ?]]

#+cats
  def flatMap[A, B](fa: FT[F, A])(f: A => FT[F, B]): FT[F, B] = monad.flatMap(fa)(f)
  def pure[A](a: A): FT[F, A] = monad.pure(a)
  override def pureEval[A](a: Eval[A]): FT[F, A] = monad.pureEval(a)
#-cats

#+scalaz
  def bind[A, B](fa: FT[F, A])(f: A => FT[F, B]): FT[F, B] = monad.bind(fa)(f)
  def point[A](a: => A): FT[F, A] = monad.point(a)
#-scalaz
}
