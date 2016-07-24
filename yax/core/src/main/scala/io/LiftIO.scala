package io

#+cats
import cats.{Applicative, Monoid}
import cats.data.{Kleisli, OptionT, StateT, WriterT, XorT => EitherT}
import cats.data.Xor.Right
#-cats

#+scalaz
import scalaz.{\/- => Right, Kleisli, OptionT, EitherT, Monad, Monoid, StateT, WriterT}
#-scalaz

trait LiftIO[F[_]] {
  def liftIO[A](io: IO[A]): F[A]
}

object LiftIO extends LiftIOInstances {
  def apply[F[_]](implicit F: LiftIO[F]): LiftIO[F] = F
}

private[io] sealed trait LiftIOInstances {
  implicit def ioLiftIoForEitherT[F[_]: LiftIO, X]: LiftIO[EitherT[F, X, ?]] = new LiftIO[EitherT[F, X, ?]] {
    def liftIO[A](io: IO[A]): EitherT[F, X, A] = EitherT(LiftIO[F].liftIO(io.map(Right(_))))
  }

  implicit def ioLiftIoForKleisli[F[_]: LiftIO, X]: LiftIO[Kleisli[F, X, ?]] = new LiftIO[Kleisli[F, X, ?]] {
    def liftIO[A](io: IO[A]): Kleisli[F, X, A] = Kleisli(_ => LiftIO[F].liftIO(io))
  }

  implicit def ioLiftIoForOptionT[F[_]: LiftIO]: LiftIO[OptionT[F, ?]] = new LiftIO[OptionT[F, ?]] {
    def liftIO[A](io: IO[A]): OptionT[F, A] = OptionT(LiftIO[F].liftIO(io.map(Some(_))))
  }

#+cats
  implicit def ioLiftIoForStateT[F[_]: LiftIO: Applicative, X]: LiftIO[StateT[F, X, ?]] = new LiftIO[StateT[F, X, ?]] {
#-cats
#+scalaz
  implicit def ioLiftIoForStateT[F[_]: LiftIO: Monad, X]: LiftIO[StateT[F, X, ?]] = new LiftIO[StateT[F, X, ?]] {
#-scalaz
    def liftIO[A](io: IO[A]): StateT[F, X, A] = StateT(state => LiftIO[F].liftIO(io.map(a => (state, a))))
  }

  implicit def ioLiftIoForWriterT[F[_]: LiftIO, X: Monoid]: LiftIO[WriterT[F, X, ?]] = new LiftIO[WriterT[F, X, ?]] {
#+cats
    val emptyX = Monoid[X].empty
#-cats
#+scalaz
    val emptyX = Monoid[X].zero
#-scalaz
    def liftIO[A](io: IO[A]): WriterT[F, X, A] = WriterT(LiftIO[F].liftIO(io.map(a => (emptyX, a))))
  }
}
