package aiyou

#+cats
import cats.Monad
#-cats

#+scalaz
import scalaz.Monad
#-scalaz

/** Type class for effects that can have IO. */
trait MonadIO[F[_]] {
  def monad: Monad[F]

  /** Lift an IO action into the effect */
  def liftIO[A](io: IO[A]): F[A]
}

object MonadIO {
  def apply[F[_]](implicit F: MonadIO[F]): MonadIO[F] = F
}
