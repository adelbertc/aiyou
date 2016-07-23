package io

sealed abstract class IORef[A] {
  protected var value: A

  def read: IO[A] = IO.primitive(value)

  def write(a: A): IO[Unit] = IO.primitive(value = a)

  def modify(f: A => A): IO[Unit] = for {
    a <- read
    _ <- write(f(a))
  } yield ()
}

object IORef {
  def create[A](a: A): IO[IORef[A]] = IO.primitive(new IORef[A] { protected var value = a })
}
