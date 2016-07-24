package io

import java.util.concurrent.Callable

import scala.annotation.tailrec

#+cats
import cats.{Eval, Monad}
import cats.data.{Xor => Either}
import cats.data.Xor.{Left, Right}
#-cats

#+scalaz
import scalaz.{BindRec, Monad, Catchable}
import scalaz.{\/ => Either, -\/ => Left, \/- => Right}
import scalaz.effect.{IO => IOz, MonadCatchIO => MonadCatchz, MonadIO => MonadIOz}
#-scalaz

sealed abstract class IO[A] { self =>
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

  def forever: IO[A] = flatMap(_ => this)

  def attempt: IO[Either[Throwable, A]] =
    IO.primitive(try Right(unsafePerformIO) catch { case t: Throwable => Left(t) })

  def attemptSome[B](p: PartialFunction[Throwable, B]): IO[Either[B, A]] =
    attempt.map(_.leftMap(e => p.lift(e).getOrElse(throw e)))

  /** Executes the handler, for exceptions propagating from this action`. */
  def except(handler: Throwable => IO[A]): IO[A] =
    attempt.flatMap(e => e.bimap(handler, IO.pure).merge)

  /** Executes the handler where defined, for exceptions propagating from this action. */
  def exceptSome(pf: PartialFunction[Throwable, IO[A]]): IO[A] =
    except(e => pf.lift(e).getOrElse((throw e)))

  /** Like "ensuring", but only performs the final action if there was an exception. */
  def onException[B](action: IO[B]): IO[A] =
    except(e => action.flatMap(_ => IO.fail(e)))

  /** Always execute `sequel` following this action; generalizes `finally`. */
  def ensuring[B](sequel: IO[B]): IO[A] =
    onException(sequel).flatMap(a => sequel.map(_ => a))

  def lift[F[_]: LiftIO]: F[A] = LiftIO[F].liftIO(this)

  def void: IO[Unit] =
    map(_ => ())

  def toCallable: Callable[A] = new Callable[A] { def call(): A = unsafePerformIO() }
}

object IO extends IOInstances with IOFunctions {
  def pure[A](a: A): IO[A] =
    Pure(a)

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
#+cats
  implicit val ioInstancesForIO: MonadCatch[IO] with MonadIO[IO] = new MonadCatch[IO] with MonadIO[IO] {
#-cats

#+scalaz
  implicit val ioInstancesForIO: BindRec[IO] with MonadCatch[IO] with MonadCatchz[IO] with MonadIO[IO] with MonadIOz[IO] =
    new BindRec[IO] with MonadCatch[IO] with MonadCatchz[IO] with MonadIO[IO] with MonadIOz[IO] {
      def tailrecM[A, B](f: A => IO[Either[A, B]])(a: A): IO[B] =
        f(a).flatMap(_ match {
          case Left(a)  => tailrecM(f)(a) // OK because trampoline
          case Right(b) => IO.pure(b)
        })
      def liftIO[A](ioa: IOz[A]): IO[A] = IO.primitive(ioa.unsafePerformIO())
#-scalaz
      def except[A](fa: IO[A])(handler: Throwable => IO[A]): IO[A] = fa.except(handler)
      def liftIO[A](io: IO[A]): IO[A] = io
#+cats
      def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa.flatMap(f)
      def pure[A](a: A): IO[A] = IO.pure(a)
      override def pureEval[A](a: Eval[A]): IO[A] = IO.primitive(a.value)
#-cats

#+scalaz
      def bind[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa.flatMap(f)
      def point[A](a: => A): IO[A] = IO.pure(a)
#-scalaz

      override def map[A, B](fa: IO[A])(f: A => B): IO[B] = fa.map(f)
    }

#+scalaz
  implicit val ioCatchableForIO: Catchable[IO] =
    new Catchable[IO] {
      def attempt[A](fa: IO[A]): IO[Either[Throwable, A]] = fa.attempt
      def fail[A](t: Throwable): IO[A] = IO.fail(t)
    }
#-scalaz
}

private[io] sealed trait IOFunctions {
  val unit: IO[Unit] =
    IO.pure(())

  def fail[A](t: Throwable): IO[A] =
    IO.primitive(throw t)
}
