package io
package syntax

trait MonadThrowSyntax {
  implicit def ioSyntaxMonadThrow(e: Throwable): MonadThrowOps =
    new MonadThrowOps(e)
}

final class MonadThrowOps(val e: Throwable) extends AnyVal {
  def throwM[F[_], A](implicit F: MonadThrow[F]): F[A] = F.throwM(e)
}
