package io
package laws

#+cats
import cats.data.{Xor => Either}
import cats.data.Xor.{Left, Right}
import cats.kernel.{Eq => Equal}
#-cats

#+scalaz
import scalaz.{\/ => Either, -\/ => Left, \/- => Right, Equal}
#-scalaz

trait Eq[A] { outer =>
  def eqv(x: A, y: A): Boolean

  def by[B](f: B => A): Eq[B] = new Eq[B] {
    def eqv(x: B, y: B): Boolean =
      outer.eqv(f(x), f(y))
  }
}

object Eq extends EqInstances0 {
  def apply[A](implicit A: Eq[A]): Eq[A] = A
}

private[laws] sealed trait EqInstances0 extends EqInstances1 {
  implicit def ioLawsEqForIO[A](implicit A: Eq[A]): Eq[IO[A]] = new Eq[IO[A]] {
    def eqv(x: IO[A], y: IO[A]): Boolean =
      (x.attempt.unsafePerformIO(), y.attempt.unsafePerformIO()) match {
        case (Left(t1), Left(t2))   => Eq[Throwable].eqv(t1, t2)
        case (Right(a1), Right(a2)) => A.eqv(a1, a2)
        case _                      => false
      }
  }

  implicit val ioLawsEqForThrowable: Eq[Throwable] = new Eq[Throwable] {
    def eqv(x: Throwable, y: Throwable): Boolean =
      x.getMessage == y.getMessage
  }
}

private[laws] sealed trait EqInstances1 {
  implicit def ioLawsEqualToEq[A](implicit A: Equal[A]): Eq[A] = new Eq[A] {
    def eqv(x: A, y: A): Boolean =
#+cats
      A.eqv(x, y)
#-cats

#+scalaz
      A.equal(x, y)
#-scalaz
  }

  implicit def ioLawsEqToEqual[A](implicit A: Eq[A]): Equal[A] = new Equal[A] {
#+cats
    def eqv(x: A, y: A): Boolean =
#-cats

#+scalaz
    def equal(x: A, y: A): Boolean =
#-scalaz
      A.eqv(x, y)
  }
}
