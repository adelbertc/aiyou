package io

#+cats
import cats.{Eval, Monad, Monoid}
import cats.data.{Kleisli, OptionT, StateT, WriterT, XorT => EitherT}
#-cats

#+scalaz
import scalaz.{Kleisli, OptionT, EitherT, Monad, Monoid, StateT, WriterT}
#-scalaz

trait MonadIO[F[_]] extends Monad[F] with LiftIO[F]

object MonadIO extends MonadIOInstances {
  def apply[F[_]](implicit F: MonadIO[F]): MonadIO[F] = F
}

private[io] sealed trait MonadIOInstances {
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

  // Annotating return types here causes implicit resolution to
  // pick up the instance itself and recurse indefinitely

  implicit def ioMonadIOForEitherT[F[_]: MonadIO, X] = derive[EitherT[F, X, ?]]

  implicit def ioMonadIOForKleisli[F[_]: MonadIO, X] = derive[Kleisli[F, X, ?]]

  implicit def ioMonadIOForOptionT[F[_]: MonadIO] = derive[OptionT[F, ?]]

  implicit def ioMonadIOForStateT[F[_]: MonadIO, X] = derive[StateT[F, X, ?]]

  implicit def ioMonadIOForWriterT[F[_]: MonadIO, X: Monoid] = derive[WriterT[F, X, ?]]
}
