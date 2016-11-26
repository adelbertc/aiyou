package aiyou

/** Type class for effects that can catch and handle exceptions. */
trait MonadCatch[F[_]] {
  def monadThrow: MonadThrow[F]

  /** Attempt to run `fa`, handling exceptions in the process. */
  def catchM[A](fa: F[A])(handler: Throwable => F[A]): F[A]

  private def flatmap[A, B](fa: F[A])(f: A => F[B]): F[B] =
    monadThrow.monad.
#+cats
    flatMap(fa)(f)
#-cats

#+scalaz
    bind(fa)(f)
#-scalaz

  def onException[A, B](fa: F[A], action: F[B]): F[A] =
    catchM(fa)(t => flatmap[B, A](action)(_ => throw t))

  def bracket[A, B, C](before: F[A])(during: A => F[B])(after: A => F[C]): F[B] =
    flatmap(before) { a =>
      flatmap(onException(during(a), after(a))) { r =>
        monadThrow.monad.map(after(a))(_ => r)
      }
    }

  def bracket_[A, B, C](before: F[A])(during: F[B])(after: F[C]): F[B] =
    bracket(before)(_ => during)(_ => after)

  def bracketOnError[A, B, C](before: F[A])(during: A => F[B])(after: A => F[C]): F[B] =
    flatmap(before)(a => onException(during(a), after(a)))

  def ensuring[A, B](fa: F[A], after: F[B]): F[A] =
    flatmap(onException(fa, after))(r => monadThrow.monad.map(after)(_ => r))
}

object MonadCatch {
  def apply[F[_]](implicit F: MonadCatch[F]): MonadCatch[F] = F
}
