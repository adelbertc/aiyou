package io.compat

import scala.language.higherKinds

trait Eithery[E[_, _]] {
  def  left[A, B](a: A): E[A, B]
  def right[A, B](b: B): E[A, B]

  def  fold[A, B, C](e: E[A, B])(f: A => C, g: B => C): C

  def bimap[A, B, C, D](e: E[A, B])(f: A => C, g: B => D): E[C, D] =
    fold(e)(a => left(f(a)), b => right(g(b)))

  def leftMap[A, B, C](e: E[A, B])(f: A => C): E[C, B] =
    bimap(e)(f, identity)

  def map[A, B, C](e: E[A, B])(f: B => C): E[A, C] =
    bimap(e)(identity, f)

  def merge[A](e: E[A, A]): A =
    fold(e)(identity, identity)

}
