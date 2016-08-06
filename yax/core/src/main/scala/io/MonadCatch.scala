package io

#+cats
import cats.{Eval, Monad, Monoid}
import cats.data.{Kleisli, OptionT, StateT, WriterT, Xor => Either, XorT => EitherT}
#-cats

#+scalaz
import scalaz.{\/ => Either, EitherT, Kleisli, OptionT, Monad, MonadError, Monoid, StateT, WriterT}
#-scalaz

import scala.util.{Either => SEither, Left => SLeft, Right => SRight}

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
  implicit def ioMonadCatchForEither: MonadCatch[Either[Throwable, ?]] =
    new MonadCatchInstance[Lambda[(F[_], A) => Either[Throwable, A]], Any] {
      val monad = MonadThrow.ioMonadThrowForEither

      def except[A](fa: Either[Throwable, A])(handler: Throwable => Either[Throwable, A]): Either[Throwable, A] =
        fa.recoverWith[Throwable, A] { case t => handler(t) }
    }

  implicit def ioMonadCatchForEitherT[F[_]: MonadCatch, X]: MonadCatch[EitherT[F, X, ?]] =
    new MonadCatchInstance[EitherT[?[_], X, ?], F] {
      val monad = MonadThrow.ioMonadThrowForEitherT[F, X]

      def except[A](fa: EitherT[F, X, A])(handler: Throwable => EitherT[F, X, A]): EitherT[F, X, A] = {
        def unwrap[G[_], Y, Z](eithert: EitherT[G, Y, Z]): G[Either[Y, Z]] =
#+cats
          eithert.value
#-cats

#+scalaz
          eithert.run
#-scalaz

        EitherT(MonadCatch[F].except(unwrap(fa))(t => unwrap(handler(t))))
      }
    }

  implicit def ioMonadCatchForSEither: MonadCatch[SEither[Throwable, ?]] =
    new MonadCatchInstance[Lambda[(F[_], A) => SEither[Throwable, A]], Any] {
      val monad = MonadThrow.ioMonadThrowForSEither

      def except[A](fa: SEither[Throwable, A])(handler: Throwable => SEither[Throwable, A]): SEither[Throwable, A] =
        fa match {
          case SLeft(e)    => handler(e)
          case r@SRight(_) => r
        }
    }

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
