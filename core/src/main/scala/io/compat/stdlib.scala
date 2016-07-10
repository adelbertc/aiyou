package io.compat

import scala.language.higherKinds

object stdlib extends StdlibModes

trait StdlibModes {

  implicit val StdlibEithery: Eithery[Either] =
    new Eithery[Either] {
      def  left[A, B](a: A): Either[A, B] = Left(a)
      def right[A, B](b: B): Either[A, B] = Right(b)
      def  fold[A, B, C](e: Either[A, B])(f: A => C, g: B => C): C = e.fold(f, g)
    }

}
