package io

import java.util.concurrent.LinkedBlockingQueue

/** A thread-safe mutable reference protected by [[IO]]. */
sealed abstract class MVar[A] {
  /** Put the value into the variable if it is empty, waiting for it to be so if necessary. */
  def put(a: A): IO[Unit]

  /** Get the contents of the variable, emptying it in the process. If variable is empty, wait until it is not. */
  def take: IO[A]

  /** Try to put the value into the (empty) variable immediately, returning true. Return false if variable is non-empty. */
  def tryPut(a: A): IO[Boolean]

  /** Like `tryTake` but does not empty the variable. */
  def tryRead: IO[Option[A]]

  /** Try to take the value of the (filled) variable immediately, emptying it in the process. */
  def tryTake: IO[Option[A]]

  def isEmpty: IO[Boolean]

  def nonEmpty: IO[Boolean] = isEmpty.map(!_)
}

object MVar {
  def empty[A]: IO[MVar[A]] =
    IO.primitive {
      val q = new LinkedBlockingQueue[A](1)
      new MVar[A] {
        def take         = IO.primitive(q.take)
        def tryTake      = IO.primitive(Option(q.poll))
        def tryRead      = IO.primitive(Option(q.peek))
        def put(a: A)    = IO.primitive(q.put(a))
        def tryPut(a: A) = IO.primitive(q.offer(a))
        def isEmpty      = IO.primitive(q.isEmpty)
      }
    }

  def apply[A](a: A): IO[MVar[A]] =
    empty[A].flatMap(mv => mv.put(a).map(_ => mv))
}
