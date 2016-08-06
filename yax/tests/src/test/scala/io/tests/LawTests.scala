package io
package tests

import io.laws.arbitrary._
import io.laws.eq._
import io.laws.laws._

import org.scalacheck.{Arbitrary, Properties}
import org.specs2.{ScalaCheck, Specification}
import org.specs2.specification.core.Fragments

import scala.util.{Either => SEither, Random}

#+cats
import cats.Eq
import cats.data.{Kleisli, OptionT, StateT, WriterT, Xor => Either, XorT => EitherT}
import cats.implicits._
import cats.laws.discipline.MonadErrorTests
import cats.laws.discipline.arbitrary._
import org.typelevel.discipline.specs2.Discipline

class LawTests extends Specification with ScalaCheck with Discipline {
#-cats

#+scalaz
import scalaz.{\/ => Either, EitherT, Equal => Eq, Kleisli, OptionT, StateT, WriterT}
import scalaz.Scalaz._
import scalaz.scalacheck.ScalazArbitrary._
import scalaz.scalacheck.ScalazProperties.monadError

class LawTests extends Specification with ScalaCheck {
#-scalaz
  def is = s2"""
  IO
    monadError  ${ioMonadError}
    monadIO     ${ioMonadIO}
    monadCatch  ${ioMonadCatch}

  Either
    monadCatch  ${eitherMonadCatch}

  EitherT
    monadIO     ${eitherTMonadIO}
    monadCatch  ${eitherTMonadCatch}

  scala.util.Either
    monadCatch  ${seitherMonadCatch}

  Kleisli
    monadIO     ${kleisliMonadIO}
    monadCatch  ${kleisliMonadCatch}

  OptionT
    monadIO     ${optionTMonadIO}
    monadCatch  ${optionTMonadCatch}

  StateT
    monadIO     ${stateTMonadIO}
    monadCatch  ${stateTMonadCatch}

  WriterT
    monadIO     ${writerTMonadIO}
    monadCatch  ${writerTMonadCatch}
  """

  def ioMonadError =
#+cats
    checkAll("MonadError[IO]", MonadErrorTests[IO, Throwable].monadError[Int, String, Char])
#-cats

#+scalaz
    properties(monadError.laws[IO, Throwable])
#-scalaz

  def ioMonadIO = monadIOTestsFor[IO]

  def ioMonadCatch = monadCatchTestsFor[IO]

  def eitherMonadCatch = monadCatchTestsFor[Either[Throwable, ?]]

  def eitherTMonadIO = monadIOTestsFor[EitherT[IO, Int, ?]]

  def eitherTMonadCatch = monadCatchTestsFor[EitherT[IO, Int, ?]]

  def seitherMonadCatch = monadCatchTestsFor[SEither[Throwable, ?]]

  implicit def ioTestsEqForKleisliIOInt[A: Eq]: Eq[Kleisli[IO, Int, A]] =
    eqBy[Kleisli[IO, Int, A], Int => IO[A]](_.run)

  def kleisliMonadIO =
    monadIOTestsFor[Kleisli[IO, Int, ?]]

  def kleisliMonadCatch =
#+cats
    ok
#-cats

#+scalaz
    monadCatchTestsFor[Kleisli[IO, Int, ?]]
#-scalaz

  def optionTMonadIO = monadIOTestsFor[OptionT[IO, ?]]

  def optionTMonadCatch =
#+cats
    ok
#-cats

#+scalaz
    monadCatchTestsFor[OptionT[IO, ?]]
#-scalaz

  implicit def ioTestsEqForStateTIOInt[A: Eq]: Eq[StateT[IO, Int, A]] =
    eqBy[StateT[IO, Int, A], Int => IO[(Int, A)]](_.run)

  def stateTMonadIO =
    monadIOTestsFor[StateT[IO, Int, ?]]

  def stateTMonadCatch =
#+cats
    ok
#-cats

#+scalaz
    monadCatchTestsFor[StateT[IO, Int, ?]]
#-scalaz

  def writerTMonadIO = monadIOTestsFor[WriterT[IO, String, ?]]

  def writerTMonadCatch =
#+cats
    ok
#-cats

#+scalaz
    monadCatchTestsFor[WriterT[IO, Int, ?]]
#-scalaz

  def monadIOTestsFor[F[_]](implicit FEI: Eq[F[Int]], FES: Eq[F[String]], FM: MonadIO[F]) =
    properties(monadIO.laws[F, Int, String])

  def monadCatchTestsFor[F[_]](implicit FA: Arbitrary[F[Int]], FE: Eq[F[Int]], FM: MonadCatch[F]) =
    properties(monadCatch.laws[F, Int])

  def properties(ps: Properties): Fragments =
    Fragments(fragmentFactory.tab) append Fragments.foreach(ps.properties) {
      case (name, prop) => Fragments(fragmentFactory.break, fragmentFactory.example(name, prop))
    }

  implicit def ioLawsEqForIntIOFunction[A](implicit A: Eq[A]): Eq[Int => IO[A]] =
    new Eq[Int => IO[A]] {
      val tests = List.fill(500)(Random.nextInt())
#+cats
      def eqv(x: Int => IO[A], y: Int => IO[A]): Boolean = {
#-cats

#+scalaz
      def equal(x: Int => IO[A], y: Int => IO[A]): Boolean = {
#-scalaz
        val xs = tests.map(x)
        val ys = tests.map(y)
        xs.zip(ys).forall { case (x1, x2) => checkEq(x1, x2) }
      }
    }
}
