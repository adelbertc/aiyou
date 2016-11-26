package aiyou

#+cats
import cats.Monad
#-cats

#+scalaz
import scalaz.Monad
#-scalaz

trait Hierarchy extends Hierarchy.H0

object Hierarchy {
  trait H0 extends H1 {
    implicit def ioMonadCatchToMonadThrow[F[_]](implicit F: MonadCatch[F]): MonadThrow[F] = F.monadThrow
  }

  trait H1 extends H2 {
    implicit def ioMonadThrowToMonad[F[_]](implicit F: MonadThrow[F]): Monad[F] = F.monad
  }

  trait H2 {
    implicit def ioMonadIOToMonad[F[_]](implicit F: MonadIO[F]): Monad[F] = F.monad
  }
}
