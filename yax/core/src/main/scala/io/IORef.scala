package io

final class IORef[A] private(@volatile protected var value: A) {
  def read: IO[A] = IO.primitive(value)

  def write(a: A): IO[Unit] = IO.primitive(value = a)

  /** Modify the value behind the reference with the function. Please note that modify is not an atomic operation. */
  def modify(f: A => A): IO[Unit] = for {
    a <- read
    _ <- write(f(a))
  } yield ()
}

object IORef {
  def create[A](a: A): IO[IORef[A]] = IO.primitive(new IORef(a))
}
