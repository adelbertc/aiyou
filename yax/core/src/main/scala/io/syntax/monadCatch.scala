package io
package syntax

trait MonadCatchSyntax {
  implicit def ioSyntaxMonadCatch[F[_]: MonadCatch, A](fa: F[A]): MonadCatchOps[F, A] =
    new MonadCatchOps(fa)
}

final class MonadCatchOps[F[_], A](fa: F[A])(implicit F: MonadCatch[F]) {
  def catchM(f: Throwable => F[A]): F[A] = F.catchM(fa)(f)

  def onException[B](action: F[B]): F[A] = F.onException(fa, action)

  def ensuring[B](after: F[B]): F[A] = F.ensuring(fa, after)
}
