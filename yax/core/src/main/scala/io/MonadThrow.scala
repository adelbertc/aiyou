package io

#+cats
import cats.{Eval, Monad, Monoid}
import cats.data.{Kleisli, OptionT, StateT, WriterT, Xor => Either, XorT => EitherT}
import cats.std.either.{eitherInstances => seitherMonad}
#-cats

#+scalaz
import scalaz.{\/ => Either, EitherT, Kleisli, OptionT, Monad, Monoid, StateT, WriterT}
import scalaz.std.either.{eitherMonad => seitherMonad}
#-scalaz

import scala.util.{Either => SEither, Left => SLeft}

trait MonadThrow[F[_]] extends Monad[F] {
  /** Throws an exception in the effect. */
  def throwM[A](e: Throwable): F[A]
}

object MonadThrow extends MonadThrowInstances {
  def apply[F[_]](implicit F: MonadThrow[F]): MonadThrow[F] = F
}

private[io] sealed trait MonadThrowInstances {
  implicit def ioMonadThrowForEither: MonadThrow[Either[Throwable, ?]] =
    new MonadThrowInstance[Lambda[(F[_], A) => Either[Throwable, A]], Any] {
      val monad = monadInstances.either[Throwable]

      def throwM[A](e: Throwable): Either[Throwable, A] = Either.left(e)
    }

  implicit def ioMonadThrowForEitherT[F[_]: MonadThrow, X]: MonadThrow[EitherT[F, X, ?]] =
    new MonadThrowInstance[EitherT[?[_], X, ?], F] {
      val monad = monadInstances.eitherT[F, X]

      def throwM[A](e: Throwable): EitherT[F, X, A] = EitherT.left(MonadThrow[F].throwM(e))
    }

  implicit def ioMonadThrowForSEither: MonadThrow[SEither[Throwable, ?]] =
    new MonadThrowInstance[Lambda[(F[_], A) => SEither[Throwable, A]], Any] {
      val monad = monadInstances.seither[Throwable]

      def throwM[A](e: Throwable): SEither[Throwable, A] = SLeft(e)
    }

  implicit def ioMonadThrowForKleisli[F[_]: MonadThrow, X]: MonadThrow[Kleisli[F, X, ?]] =
    new MonadThrowInstance[Kleisli[?[_], X, ?], F] {
      val monad = monadInstances.kleisli[F, X]

      def throwM[A](e: Throwable): Kleisli[F, X, A] =
        Kleisli(_ => MonadThrow[F].throwM(e))
    }

  implicit def ioMonadThrowForOptionT[F[_]: MonadThrow]: MonadThrow[OptionT[F, ?]] =
    new MonadThrowInstance[OptionT, F] {
      val monad = monadInstances.optionT[F]

      def throwM[A](e: Throwable): OptionT[F, A] = OptionT(MonadThrow[F].throwM(e))
    }

  implicit def ioMonadThrowForStateT[F[_]: MonadThrow, X]: MonadThrow[StateT[F, X, ?]] =
    new MonadThrowInstance[StateT[?[_], X, ?], F] {
      val monad = monadInstances.stateT[F, X]

      def throwM[A](e: Throwable): StateT[F, X, A] = StateT(_ => MonadThrow[F].throwM(e))
    }

  implicit def ioMonadThrowForWriterT[F[_]: MonadThrow, X: Monoid]: MonadThrow[WriterT[F, X, ?]] =
    new MonadThrowInstance[WriterT[?[_], X, ?], F] {
      val monad = monadInstances.writerT[F, X]

      def throwM[A](e: Throwable): WriterT[F, X, A] = WriterT(MonadThrow[F].throwM(e))
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
