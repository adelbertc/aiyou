package aiyou

#+cats
import cats.Monad
import cats.data.OptionT
#-cats

#+scalaz
import scalaz.{Monad, OptionT}
#-scalaz

import aiyou.implicits._

object CompileTests {
  val mockIO: IO[Int] = IO.pure(5)
  val mockOptionT: OptionT[IO, Int] = OptionT(mockIO.map(i => Some(i): Option[Int]))

  def mtlAmbiguity[F[_]: MonadIO: MonadCatch]: Monad[F] = Monad[F]

  def monadThrowImpliesMonadCatch[F[_]: MonadCatch]: MonadThrow[F] = MonadThrow[F]

  def monadCatchImpliesMonad[F[_]: MonadCatch]: Monad[F] = Monad[F]

  // MonadCatch syntax
  mockOptionT.catchM(t => mockOptionT)
  mockOptionT.onException(mockOptionT)
  mockOptionT.ensuring(mockOptionT)

  // MonadThrow syntax
  val t = new Exception()
  t.throwM[IO, Int]
}
