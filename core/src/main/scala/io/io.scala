package io

import io.compat.Eithery

import scala.annotation.tailrec
import scala.language.higherKinds
import java.util.concurrent.Callable

sealed abstract class IO[A] { self =>

  def unsafePerformIO: A

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

  def attempt[E[_, _]](implicit ev: Eithery[E]): IO[E[Throwable, A]] =
    IO.primitive(try ev.right(unsafePerformIO) catch { case t: Throwable => ev.left(t) })

  /** Like `attempt` but catches (and maps) only where defined. */
  def attemptSome[E[_, _], B](p: PartialFunction[Throwable, B])(implicit ev: Eithery[E]): IO[E[B, A]] =
    attempt.map(ev.leftMap(_)(e => p.lift(e).getOrElse(throw e)))

  /** Executes the handler, for exceptions propagating from `ma`. */
  def except(handler: Throwable => IO[A]): IO[A] = {
    val E = io.compat.stdlib.StdlibEithery
    attempt(E).flatMap(e => E.merge(E.bimap(e)(handler, IO.pure)))
  }

  /** Executes the handler where defined, for exceptions propagating from `ma`. */
  def exceptSome(pf: PartialFunction[Throwable, IO[A]]): IO[A] =
    except(e => pf.lift(e).getOrElse((throw e)))

  /** Like "ensuring", but only performs the final action if there was an exception. */
  def onException[B](action: IO[B]): IO[A] =
    except(e => action.flatMap(_ => IO.fail(e)))

  /** Always execute `sequel` following `ma`; generalizes `finally`. */
  def ensuring[B](sequel: IO[B]): IO[A] =
    onException(sequel).flatMap(a => sequel.map(_ => a))

  def void: IO[Unit] =
    map(_ => ())

  def unsafeRunnable: Callable[A] =
    new Callable[A] {
      def call: A = unsafePerformIO
    }

}

object IO extends IOFunctions {

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

trait IOFunctions {

  val unit: IO[Unit] =
    IO.pure(())

  def fail[A](t: Throwable): IO[A] =
    IO.primitive(throw t)

}
