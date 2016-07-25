package io
package tests

import io.laws.Eq
import io.laws.Eq.ioLawsEqToEqual
import io.laws.arbitrary._
import io.laws.laws._

import org.scalacheck.{Arbitrary, Properties}
import org.specs2.{ScalaCheck, Specification}
import org.specs2.specification.core.Fragments

import scala.util.Random

#+cats
import cats.data.{Kleisli, OptionT, StateT, WriterT, XorT => EitherT}
import cats.implicits._
import cats.laws.discipline.MonadTests
import cats.laws.discipline.arbitrary._
import org.typelevel.discipline.specs2.Discipline

class LawTests extends Specification with ScalaCheck with Discipline {
#-cats

#+scalaz
import scalaz.{EitherT, Kleisli, OptionT, StateT, WriterT}
import scalaz.Scalaz._
import scalaz.scalacheck.ScalazArbitrary._
import scalaz.scalacheck.ScalazProperties.monad

class LawTests extends Specification with ScalaCheck {
#-scalaz
  def is = s2"""
  IO
    monad       ${ioMonad}
    monadIO     ${ioMonadIO}
    monadCatch  ${ioMonadCatch}

  EitherT
    monadIO     ${eitherTMonadIO}

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

  def ioMonad =
#+cats
    checkAll("Monad[IO]", MonadTests[IO].monad[Int, String, Char])
#-cats

#+scalaz
    properties(monad.laws[IO])
#-scalaz

  def ioMonadIO = monadIOTestsFor[IO]

  def ioMonadCatch = monadCatchTestsFor[IO]

  def eitherTMonadIO = monadIOTestsFor[EitherT[IO, Int, ?]]

  implicit def ioTestsEqForKleisliIOInt[A: Eq]: Eq[Kleisli[IO, Int, A]] = Eq[Int => IO[A]].by(_.run)

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

  implicit def ioTestsEqForStateTIOInt[A: Eq]: Eq[StateT[IO, Int, A]] = Eq[Int => IO[(Int, A)]].by(_.run)

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
      def eqv(x: Int => IO[A], y: Int => IO[A]): Boolean = {
        val xs = tests.map(x)
        val ys = tests.map(y)
        xs.zip(ys).forall { case (x1, x2) => Eq[IO[A]].eqv(x1, x2) }
      }
    }
}
