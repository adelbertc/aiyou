package io

#+cats
import cats.{Eval, Monad, Monoid}
import cats.data.{Kleisli, OptionT, StateT, WriterT, XorT => EitherT}
#-cats

#+scalaz
import scalaz.{Kleisli, OptionT, EitherT, Monad, Monoid, StateT, WriterT}
#-scalaz

trait MonadIO[F[_]] extends Monad[F] with LiftIO[F]

object MonadIO {
  def apply[F[_]](implicit F: MonadIO[F]): MonadIO[F] = F

  private def derive[F[_]](implicit MF: Monad[F], LF: LiftIO[F]): MonadIO[F] = new MonadIO[F] {
    def liftIO[A](io: IO[A]): F[A] = LF.liftIO(io)

#+cats
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = MF.flatMap(fa)(f)
    def pure[A](a: A): F[A] = MF.pure(a)
    override def pureEval[A](a: Eval[A]): F[A] = MF.pureEval(a)
#-cats

#+scalaz
    def bind[A, B](fa: F[A])(f: A => F[B]): F[B] = MF.bind(fa)(f)
    def point[A](a: => A): F[A] = MF.pure(a)
#-scalaz
  }


  implicit def ioMonadIoForEitherT[F[_]: MonadIO, X]: MonadIO[EitherT[F, X, ?]] = derive[EitherT[F, X, ?]]

  implicit def ioMonadIoForKleisli[F[_]: MonadIO, X]: MonadIO[Kleisli[F, X, ?]] = derive[Kleisli[F, X, ?]]

  implicit def ioMonadIoForOptionT[F[_]: MonadIO]: MonadIO[OptionT[F, ?]] = derive[OptionT[F, ?]]

  implicit def ioMonadIoForStateT[F[_]: MonadIO, X]: MonadIO[StateT[F, X, ?]] = derive[StateT[F, X, ?]]

  implicit def ioMonadIoForWriterT[F[_]: MonadIO, X: Monoid]: MonadIO[WriterT[F, X, ?]] = derive[WriterT[F, X, ?]]
}
