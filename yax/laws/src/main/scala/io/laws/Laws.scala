package io
package laws

#+cats
import cats.{Eq, Eval, Monad}
#-cats

#+scalaz
import scalaz.{Equal => Eq, Monad}
#-scalaz

import io.laws.arbitrary._
import io.laws.eq.checkEq

import org.scalacheck.{Arbitrary, Cogen, Prop, Properties}
import org.scalacheck.Prop.forAll

object laws {
  private def flatmap[F[_]: Monad, A, B](fa: F[A])(f: A => F[B]): F[B] =
#+cats
    Monad[F].flatMap(fa)(f)
#-cats

#+scalaz
    Monad[F].bind(fa)(f)
#-scalaz

  private def newProperties(name: String)(f: Properties => Unit): Properties = {
    val p = new Properties(name)
    f(p)
    p
  }

  object monadIO {
    def pure[F[_], A](implicit FAE: Eq[F[A]], FM: MonadIO[F], A: Arbitrary[A]): Prop = forAll { (a: A) =>
      val got = FM.liftIO(IO.pure(a))
      val expected = FM.pure(a)
      checkEq(got, expected)
    }

    def flatMap[F[_], A, B](implicit FBE: Eq[F[B]], FM: MonadIO[F], AA: Arbitrary[A], AC: Cogen[A], B: Arbitrary[B]): Prop =
      forAll { (io: IO[A], f: A => IO[B]) =>
        val got = FM.liftIO(io.flatMap(f))
        val expected = flatmap(FM.liftIO(io))(a => FM.liftIO(f(a)))
        checkEq(got, expected)
      }

    def laws[F[_], A, B](implicit FAE: Eq[F[A]], FBE: Eq[F[B]], FM: MonadIO[F], AA: Arbitrary[A], AC: Cogen[A], B: Arbitrary[B]): Properties =
      newProperties("monadIO") { p =>
        p.property("liftIO . pure = pure") = pure[F, A]
        p.property("liftIO (m flatMap f) = liftIO m flatMap (liftIO . f)") = flatMap[F, A, B]
        ()
      }
  }

  object monadThrow {
    def shortCircuit[F[_], A](implicit FAA: Arbitrary[F[A]], FAE: Eq[F[A]], FT: MonadThrow[F]): Prop =
      forAll { (e: Throwable, x: F[A]) =>
        val thrown = FT.throwM[A](e)
        checkEq(flatmap(thrown)(_ => x), thrown)
      }

    def laws[F[_], A](implicit FAA: Arbitrary[F[A]], FAE: Eq[F[A]], FT: MonadThrow[F]): Properties =
      newProperties("monadThrow") { p =>
        p.property("throwM e >> x = throwM e") = shortCircuit[F, A]
        ()
      }
  }

  object monadCatch {
    def catchM[F[_], A](implicit FAA: Arbitrary[F[A]], FAE: Eq[F[A]], FM: MonadCatch[F]): Prop =
      forAll { (e: Throwable, f: Throwable => F[A]) =>
        checkEq(FM.catchM(FM.throwM[A](e))(f), f(e))
      }

    def laws[F[_], A](implicit FAA: Arbitrary[F[A]], FAE: Eq[F[A]], FM: MonadCatch[F]): Properties =
      newProperties("monadCatch") { p =>
        p.include(monadThrow.laws[F, A])
        p.property("catch (throw e) f = f e") = catchM[F, A]
        ()
      }
  }
}
