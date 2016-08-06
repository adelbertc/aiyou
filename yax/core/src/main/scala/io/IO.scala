package io

import java.util.concurrent.Callable

import scala.annotation.tailrec

#+cats
import cats.{Eval, MonadError, Show}
import cats.data.{Xor => Either}
import cats.data.Xor.{Left, Right}
#-cats

#+scalaz
import scalaz.{BindRec, Catchable, MonadError, Show}
import scalaz.{\/ => Either, -\/ => Left, \/- => Right}
#-scalaz

/** An IO action, when run, will produce a value of type A */
sealed abstract class IO[A] { self =>
  /** Run this action, possible throwing an exception. Use `attempt` before calling this if you don't want to throw. */
  def unsafePerformIO(): A

  def map[B](f: A => B): IO[B] =
    flatMap(a => IO.Pure(f(a)))

  def flatMap[B](f: A => IO[B]): IO[B] =
    this match {
      case c: IO.Compute[A] =>
        new IO.Compute[B] {
          type Start = c.Start
          val start = c.start
          val run = (s: c.Start) =>
            new IO.Compute[B] {
              type Start = A
              val start = () => c.run(s)
              val run = f
            }
        }
      case _ =>
        new IO.Compute[B] {
          type Start = A
          val start = () => self
          val run = f
        }
    }

  /** Creates an IO action that, when run, performs the action of this action indefinitely */
  def forever: IO[A] = flatMap(_ => this)

  /** Creates an IO action that, when run, produces either a `Throwable` or the `A` */
  def attempt: IO[Either[Throwable, A]] =
    IO.primitive(try Right(unsafePerformIO()) catch { case t: Throwable => Left(t) })

  /** Like `attempt`, but transforms (a subset of) exceptions before handing it back. */
  def attemptSome[B](p: PartialFunction[Throwable, B]): IO[Either[B, A]] =
    attempt.map(_.leftMap(e => p.lift(e).getOrElse(throw e)))

  /** Executes the handler, for exceptions propagating from this action. */
  def catchM(handler: Throwable => IO[A]): IO[A] =
    attempt.flatMap(e => e.bimap(handler, IO.pure).merge)

  /** Executes the handler where defined, for exceptions propagating from this action. */
  def catchMSome(pf: PartialFunction[Throwable, IO[A]]): IO[A] =
    catchM(e => pf.lift(e).getOrElse((throw e)))

  /** Like "ensuring", but only performs the final action if there was an exception. */
  def onException[B](action: IO[B]): IO[A] =
    catchM(e => action.flatMap(_ => IO.fail(e)))

  /** Always execute `sequel` following this action. */
  def ensuring[B](sequel: IO[B]): IO[A] =
    onException(sequel).flatMap(a => sequel.map(_ => a))

  /** Lift this action into another effect. */
  def liftIO[F[_]: MonadIO]: F[A] = MonadIO[F].liftIO(this)

  def void: IO[Unit] =
    map(_ => ())

  def toCallable: Callable[A] = new Callable[A] { def call(): A = unsafePerformIO() }
}

object IO extends IOInstances with IOFunctions {
  /** Lift a pure value into [[IO]]. */
  def pure[A](a: A): IO[A] =
    Pure(a)

  /** Lift an impure value into [[IO]]. */
  def primitive[A](a: => A): IO[A] =
    new Primitive(a _)

  private final case class Pure[A](unsafePerformIO: A) extends IO[A]

  private final class Primitive[A](f: () => A) extends IO[A] {
    def unsafePerformIO: A = f()
  }

  private sealed abstract class Compute[A] extends IO[A] {
    type Start
    val start: () => IO[Start]
    val run: Start => IO[A]
    def unsafePerformIO: A = {
      type L = IO[Any]
      type C = Any => IO[Any]
      @tailrec def loop(curr: L, fs: List[C]): Any =
        curr match {
          case c: Compute[_] =>
            c.start() match {
              case cc: Compute[_] =>
                loop(
                  cc.start().asInstanceOf[L],
                  cc.run.asInstanceOf[C] :: c.run.asInstanceOf[C] :: fs)
              case xx =>
                loop(c.run(xx.unsafePerformIO).asInstanceOf[L], fs)
            }
          case x =>
            fs match {
              case f :: fs => loop(f(x.unsafePerformIO), fs)
              case Nil => x.unsafePerformIO
            }
        }
      loop(this.asInstanceOf[L], Nil).asInstanceOf[A]
    }
  }

}

private[io] sealed trait IOInstances {
#+scalaz
  implicit val ioInstancesForIO:
        BindRec[IO] with Catchable[IO] with MonadCatch[IO] with MonadError[IO, Throwable] with MonadIO[IO] =
    new BindRec[IO] with Catchable[IO] with MonadCatch[IO] with MonadError[IO, Throwable] with MonadIO[IO] {
#-scalaz

#+cats
  implicit val ioInstancesForIO: MonadCatch[IO] with MonadError[IO, Throwable] with MonadIO[IO] =
    new MonadCatch[IO] with MonadError[IO, Throwable] with MonadIO[IO] {
      def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa.flatMap(f)
      def pure[A](a: A): IO[A] = IO.pure(a)
#-cats

#+scalaz
      def tailrecM[A, B](f: A => IO[Either[A, B]])(a: A): IO[B] =
        f(a).flatMap(_ match {
          case Left(a)  => tailrecM(f)(a) // OK because trampoline
          case Right(b) => IO.pure(b)
        })
      def attempt[A](fa: IO[A]): IO[Either[Throwable, A]] = fa.attempt
      def fail[A](t: Throwable): IO[A] = IO.fail(t)
      def bind[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa.flatMap(f)
      def point[A](a: => A): IO[A] = IO.pure(a)
#-scalaz

#+cats
      def handleErrorWith[A](fa: IO[A])(f: Throwable => IO[A]): IO[A] =
#-cats

#+scalaz
      def handleError[A](fa: IO[A])(f: Throwable => IO[A]): IO[A] =
#-scalaz

        catchM(fa)(f)

      def raiseError[A](e: Throwable): IO[A] = IO.fail(e)

      def catchM[A](fa: IO[A])(handler: Throwable => IO[A]): IO[A] = fa.catchM(handler)
      def throwM[A](throwable: Throwable): IO[A] = IO.fail(throwable)
      def liftIO[A](io: IO[A]): IO[A] = io
      override def map[A, B](fa: IO[A])(f: A => B): IO[B] = fa.map(f)
    }
}

private[io] sealed trait IOFunctions {
  def print(s: String): IO[Unit] = IO.primitive {
    Predef.print(s)
    ()
  }

  def println(s: String): IO[Unit] = IO.primitive {
    Predef.println(s)
    ()
  }

  private def show[A](a: A)(implicit A: Show[A]): String =
#+cats
    A.show(a)
#-cats

#+scalaz
    A.shows(a)
#-scalaz

  def put[A: Show](a: A): IO[Unit] = print(show(a))

  def putLn[A: Show](a: A): IO[Unit] = println(show(a))

  val unit: IO[Unit] =
    IO.pure(())

  def fail[A](t: Throwable): IO[A] =
    IO.primitive(throw t)
}
