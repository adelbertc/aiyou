package io

// an MVar simply wraps a LinkedBlockingQueue with a length of 1

import java.util.concurrent.LinkedBlockingQueue

// all operations are atomic
trait MVar[A] {
  def put(a: A): IO[Unit]

  def take: IO[A]

  def tryPut(a: A): IO[Boolean]

  def tryRead: IO[Option[A]]

  def tryTake: IO[Option[A]]

  def isEmpty: IO[Boolean]

}

object MVar {
  def newEmptyMVar[A]: IO[MVar[A]] =
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

  def newMVar[A](a: A): IO[MVar[A]] =
    newEmptyMVar[A].flatMap(mv => mv.put(a).map(_ => mv))
}
