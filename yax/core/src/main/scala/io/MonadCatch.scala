package io

#+cats
import cats.{Eval, Monad, Monoid}
import cats.data.{Kleisli, OptionT, StateT, WriterT, XorT => EitherT}
#-cats

#+scalaz
import scalaz.{Kleisli, OptionT, EitherT, Monad, MonadError, Monoid, StateT, WriterT}
#-scalaz

trait MonadCatch[F[_]] extends Monad[F] {
  def except[A](fa: F[A])(f: Throwable => F[A]): F[A]

  private def flatmap[A, B](fa: F[A])(f: A => F[B]): F[B] =
#+cats
    flatMap(fa)(f)
#-cats

#+scalaz
    bind(fa)(f)
#-scalaz

  def onException[A, B](fa: F[A], action: F[B]): F[A] =
    except(fa)(t => flatmap[B, A](action)(_ => throw t))

  def bracket[A, B, C](before: F[A])(during: A => F[B])(after: A => F[C]): F[B] =
    flatmap(before) { a =>
      flatmap(onException(during(a), after(a))) { r =>
        map(after(a))(_ => r)
      }
    }

  def bracket_[A, B, C](before: F[A])(during: F[B])(after: F[C]): F[B] =
    bracket(before)(_ => during)(_ => after)

  def bracketOnError[A, B, C](before: F[A])(during: A => F[B])(after: A => F[C]): F[B] =
    flatmap(before)(a => onException(during(a), after(a)))

  def ensuring[A, B](fa: F[A], after: F[B]): F[A] =
    flatmap(onException(fa, after))(r => map(after)(_ => r))
}

object MonadCatch extends MonadCatchInstances {
  def apply[F[_]](implicit F: MonadCatch[F]): MonadCatch[F] = F
}

private[io] sealed trait MonadCatchInstances {
  implicit def ioMonadCatchForEitherT[F[_]: MonadCatch]: MonadCatch[EitherT[F, Throwable, ?]] =
    new MonadCatchInstance[EitherT[?[_], Throwable, ?], F] {
      val monad =
#+cats
        EitherT.xorTMonadError[F, Throwable]
#-cats

#+scalaz
        EitherT.eitherTMonadError[F, Throwable]
#-scalaz
      def except[A](fa: EitherT[F, Throwable, A])(f: Throwable => EitherT[F, Throwable, A]): EitherT[F, Throwable, A] =
#+cats
        fa.recoverWith { case t: Throwable => f(t) }
#-cats

#+scalaz
        MonadError[EitherT[F, Throwable, ?], Throwable].handleError(fa)(f)
#-scalaz
    }

  implicit def ioMonadCatchForKleisli[F[_]: MonadCatch, X]: MonadCatch[Kleisli[F, X, ?]] =
    new MonadCatchInstance[Kleisli[?[_], X, ?], F] {
      val monad =
#+cats
        Kleisli.kleisliMonadReader[F, X]
#-cats

#+scalaz
        Kleisli.kleisliMonadReader[F, X]
#-scalaz

      def except[A](fa: Kleisli[F, X, A])(handler: Throwable => Kleisli[F, X, A]): Kleisli[F, X, A] =
        Kleisli((x: X) => MonadCatch[F].except(fa.run(x))(t => handler(t).run(x)))
    }

  implicit def ioMonadCatchForOptionT[F[_]: MonadCatch]: MonadCatch[OptionT[F, ?]] =
    new MonadCatchInstance[OptionT, F] {
      val monad =
#+cats
        OptionT.optionTMonad[F]
#-cats

#+scalaz
        OptionT.optionTMonadPlus[F]
#-scalaz

      def except[A](fa: OptionT[F, A])(handler: Throwable => OptionT[F, A]): OptionT[F, A] = {
        def unwrap[G[_], Y](optiont: OptionT[G, Y]): G[Option[Y]] =
#+cats
          optiont.value
#-cats

#+scalaz
          optiont.run
#-scalaz
        OptionT(MonadCatch[F].except(unwrap(fa))(t => unwrap(handler(t))))
      }
    }

  implicit def ioMonadCatchForStateT[F[_]: MonadCatch, X]: MonadCatch[StateT[F, X, ?]] =
    new MonadCatchInstance[StateT[?[_], X, ?], F] {
      val monad =
#+cats
        StateT.stateTMonadState[F, X]
#-cats

#+scalaz
        StateT.stateTMonadState[X, F]
#-scalaz

      def except[A](fa: StateT[F, X, A])(handler: Throwable => StateT[F, X, A]): StateT[F, X, A] =
        StateT(x => MonadCatch[F].except(fa.run(x))(t => handler(t).run(x)))
    }

  implicit def ioMonadCatchForWriterT[F[_]: MonadCatch, X: Monoid]: MonadCatch[WriterT[F, X, ?]] =
    new MonadCatchInstance[WriterT[?[_], X, ?], F] {
      val monad =
#+cats
        WriterT.writerTMonadWriter[F, X]
#-cats

#+scalaz
        WriterT.writerTMonadListen[F, X]
#-scalaz

      def except[A](fa: WriterT[F, X, A])(handler: Throwable => WriterT[F, X, A]): WriterT[F, X, A] =
        WriterT(MonadCatch[F].except(fa.run)(t => handler(t).run))
    }
}

private[io] trait MonadCatchInstance[FT[_[_], _], F[_]] extends MonadCatch[FT[F, ?]] {
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