package io

#+cats
import cats.{Applicative, Eval, Monad, Monoid}
import cats.data.{Kleisli, OptionT, StateT, WriterT, XorT => EitherT}
import cats.data.Xor.Right
#-cats

#+scalaz
import scalaz.{\/- => Right, Kleisli, OptionT, EitherT, Monad, Monoid, StateT, WriterT}
#-scalaz

/** Type class for effects that can have IO. */
trait MonadIO[F[_]] extends Monad[F] {
  /** Lift an IO action into the effect */
  def liftIO[A](io: IO[A]): F[A]
}

object MonadIO extends MonadIOInstances {
  def apply[F[_]](implicit F: MonadIO[F]): MonadIO[F] = F
}

private[io] sealed trait MonadIOInstances {
  implicit def ioMonadIOForEitherT[F[_]: MonadIO, X]: MonadIO[EitherT[F, X, ?]] =
    new MonadIOInstance[EitherT[?[_], X, ?], F] {
      def monad = monadInstances.eitherT[F, X]

      def liftIO[A](io: IO[A]): EitherT[F, X, A] = EitherT(MonadIO[F].liftIO(io.map(Right(_))))
    }

  implicit def ioMonadIOForKleisli[F[_]: MonadIO, X]: MonadIO[Kleisli[F, X, ?]] =
    new MonadIOInstance[Kleisli[?[_], X, ?], F] {
      def monad = monadInstances.kleisli[F, X]

      def liftIO[A](io: IO[A]): Kleisli[F, X, A] = Kleisli(_ => MonadIO[F].liftIO(io))
    }

  implicit def ioMonadIOForOptionT[F[_]: MonadIO]: MonadIO[OptionT[F, ?]] =
    new MonadIOInstance[OptionT, F] {
      def monad = monadInstances.optionT[F]

      def liftIO[A](io: IO[A]): OptionT[F, A] = OptionT(MonadIO[F].liftIO(io.map(Some(_))))
    }

#+cats
  implicit def ioMonadIOForStateT[F[_]: MonadIO: Applicative, X]: MonadIO[StateT[F, X, ?]] =
    new MonadIOInstance[StateT[?[_], X, ?], F] {
#-cats

#+scalaz
  implicit def ioMonadIOForStateT[F[_]: MonadIO: Monad, X]: MonadIO[StateT[F, X, ?]] =
    new MonadIOInstance[StateT[?[_], X, ?], F] {
#-scalaz
      def monad = monadInstances.stateT[F, X]

      def liftIO[A](io: IO[A]): StateT[F, X, A] = StateT(state => MonadIO[F].liftIO(io.map(a => (state, a))))
    }

  implicit def ioMonadIOForWriterT[F[_]: MonadIO, X: Monoid]: MonadIO[WriterT[F, X, ?]] =
    new MonadIOInstance[WriterT[?[_], X, ?], F] {
#+cats
      val emptyX = Monoid[X].empty
#-cats

#+scalaz
      val emptyX = Monoid[X].zero
#-scalaz
      def monad = monadInstances.writerT[F, X]

      def liftIO[A](io: IO[A]): WriterT[F, X, A] = WriterT(MonadIO[F].liftIO(io.map(a => (emptyX, a))))
    }
}

private[io] trait MonadIOInstance[FT[_[_], _], F[_]] extends MonadIO[FT[F, ?]] {
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
