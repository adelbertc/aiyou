package aiyou
package laws

#+cats
import cats.kernel.Eq
#-cats

#+scalaz
import scalaz.{Equal => Eq}
import scalaz.{-\/ => Left, \/- => Right}
#-scalaz

object eq {
  def checkEq[A](x: A, y: A)(implicit A: Eq[A]): Boolean =
#+cats
    A.eqv(x, y)
#-cats

#+scalaz
    A.equal(x, y)
#-scalaz

  def eqBy[A, B](f: A => B)(implicit B: Eq[B]): Eq[A] = new Eq[A] {
#+cats
    def eqv(x: A, y: A): Boolean =
#-cats

#+scalaz
    def equal(x: A, y: A): Boolean =
#-scalaz
      checkEq(f(x), f(y))
  }

  def instance[A](f: (A, A) => Boolean): Eq[A] = new Eq[A] {
#+cats
    def eqv(x: A, y: A): Boolean =
#-cats

#+scalaz
    def equal(x: A, y: A): Boolean =
#-scalaz
      f(x, y)
  }

  implicit def ioLawsEqForIO[A: Eq]: Eq[IO[A]] = new Eq[IO[A]] {
#+cats
    def eqv(x: IO[A], y: IO[A]): Boolean =
#-cats

#+scalaz
    def equal(x: IO[A], y: IO[A]): Boolean =
#-scalaz
      (x.attempt.unsafePerformIO(), y.attempt.unsafePerformIO()) match {
        case (Left(t1), Left(t2))   => checkEq(t1, t2)
        case (Right(a1), Right(a2)) => checkEq(a1, a2)
        case _                      => false
      }
  }

  implicit val ioLawsEqForThrowable: Eq[Throwable] = new Eq[Throwable] {
#+cats
    def eqv(x: Throwable, y: Throwable): Boolean =
#-cats

#+scalaz
    def equal(x: Throwable, y: Throwable): Boolean =
#-scalaz
      x.getMessage == y.getMessage
  }
}
