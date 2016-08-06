package io

#+cats
import cats.{Eval, Monad, Monoid}
import cats.data.{Kleisli, OptionT, StateT, WriterT}
#-cats

#+scalaz
import scalaz.{Kleisli, OptionT, Monad, Monoid, StateT, WriterT}
#-scalaz

/** Type class for effects that can catch and handle exceptions. */
trait MonadCatch[F[_]] extends MonadThrow[F] {
  /** Attempt to run `fa`, handling exceptions in the process. */
  def except[A](fa: F[A])(handler: Throwable => F[A]): F[A]

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
  implicit def ioMonadCatchForKleisli[F[_]: MonadCatch, X]: MonadCatch[Kleisli[F, X, ?]] =
    new MonadCatchInstance[Kleisli[?[_], X, ?], F] {
      val monad = MonadThrow.ioMonadThrowForKleisli[F, X]

      def except[A](fa: Kleisli[F, X, A])(handler: Throwable => Kleisli[F, X, A]): Kleisli[F, X, A] =
        Kleisli((x: X) => MonadCatch[F].except(fa.run(x))(t => handler(t).run(x)))
    }

  implicit def ioMonadCatchForOptionT[F[_]: MonadCatch]: MonadCatch[OptionT[F, ?]] =
    new MonadCatchInstance[OptionT, F] {
      val monad = MonadThrow.ioMonadThrowForOptionT[F]

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
      val monad = MonadThrow.ioMonadThrowForStateT[F, X]

      def except[A](fa: StateT[F, X, A])(handler: Throwable => StateT[F, X, A]): StateT[F, X, A] =
        StateT(x => MonadCatch[F].except(fa.run(x))(t => handler(t).run(x)))
    }

  implicit def ioMonadCatchForWriterT[F[_]: MonadCatch, X: Monoid]: MonadCatch[WriterT[F, X, ?]] =
    new MonadCatchInstance[WriterT[?[_], X, ?], F] {
      val monad = MonadThrow.ioMonadThrowForWriterT[F, X]

      def except[A](fa: WriterT[F, X, A])(handler: Throwable => WriterT[F, X, A]): WriterT[F, X, A] =
        WriterT(MonadCatch[F].except(fa.run)(t => handler(t).run))
    }
}

private[io] trait MonadCatchInstance[FT[_[_], _], F[_]] extends MonadCatch[FT[F, ?]] with MonadThrowInstance[FT, F] {
  override def monad: MonadThrow[FT[F, ?]]

  def throwM[A](e: Throwable): FT[F, A] = monad.throwM(e)
}
