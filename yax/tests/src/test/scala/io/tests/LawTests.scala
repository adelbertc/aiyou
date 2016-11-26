package io
package tests

import io.implicits._
import io.laws.arbitrary._
import io.laws.eq._
import io.laws.laws._

import org.scalacheck.{Arbitrary, Properties}
import org.specs2.{ScalaCheck, Specification}
import org.specs2.specification.core.Fragments

import scala.util.Random

#+cats
import cats.Eq
import cats.Applicative
import cats.data.{EitherT, Kleisli, OptionT, StateT, WriterT}
import cats.kernel.laws.GroupLaws
import cats.implicits._
import cats.laws.discipline.MonadErrorTests
import cats.laws.discipline.arbitrary._
import org.typelevel.discipline.specs2.Discipline

class LawTests extends Specification with ScalaCheck with Discipline {
#-cats

#+scalaz
import scalaz.{\/, EitherT, Equal => Eq, Kleisli, OptionT, StateT, WriterT}
import scalaz.Scalaz._
import scalaz.scalacheck.ScalazArbitrary._
import scalaz.scalacheck.ScalazProperties.{monadError, monoid}

class LawTests extends Specification with ScalaCheck {
#-scalaz
  def is = s2"""
  IO
    monadError  ${ioMonadError}
    monadIO     ${ioMonadIO}
    monadCatch  ${ioMonadCatch}
    monoid      ${ioMonoid}

  Either
    monadCatch  ${eitherMonadCatch}

#+scalaz
  \/
    monadCatch  ${disjMonadCatch}
#-scalaz

  EitherT
    monadIO     ${eitherTMonadIO}
    monadCatch  ${eitherTMonadCatch}

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
    mkProp(monadError.laws[IO, Throwable])
#-scalaz

  def ioMonadIO = monadIOTestsFor[IO]

  def ioMonadCatch = monadCatchTestsFor[IO]

  def ioMonoid =
#+cats
    checkAll("Monoid[IO[A]]", GroupLaws[IO[Int]].monoid)
#-cats

#+scalaz
    mkProp(monoid.laws[IO[Int]])
#-scalaz

  def eitherMonadCatch = monadCatchTestsFor[Either[Throwable, ?]]

#+scalaz
  def disjMonadCatch = monadCatchTestsFor[\/[Throwable, ?]]
#-scalaz

  def eitherTMonadIO = monadIOTestsFor[EitherT[IO, Int, ?]]

  def eitherTMonadCatch = monadCatchTestsFor[EitherT[IO, Int, ?]]

  implicit def ioTestsEqForKleisliIOInt[A: Eq]: Eq[Kleisli[IO, Int, A]] =
    eqBy[Kleisli[IO, Int, A], Int => IO[A]](_.run)

  def kleisliMonadIO = monadIOTestsFor[Kleisli[IO, Int, ?]]

  def kleisliMonadCatch = monadCatchTestsFor[Kleisli[IO, Int, ?]]

  def optionTMonadIO = monadIOTestsFor[OptionT[IO, ?]]

  def optionTMonadCatch = monadCatchTestsFor[OptionT[IO, ?]]

  implicit def ioTestsEqForStateTIOInt[A: Eq]: Eq[StateT[IO, Int, A]] =
    eqBy[StateT[IO, Int, A], Int => IO[(Int, A)]](_.run)

#+cats
  implicit def ioLawsArbitraryForCatsStateT[F[_], S, A](implicit FApp: Applicative[F], FArb: Arbitrary[S => F[(S, A)]]): Arbitrary[StateT[F, S, A]] =
    Arbitrary(FArb.arbitrary.map(StateT(_)))
#-cats

  def stateTMonadIO = monadIOTestsFor[StateT[IO, Int, ?]]

  def stateTMonadCatch = monadCatchTestsFor[StateT[IO, Int, ?]]

  def writerTMonadIO = monadIOTestsFor[WriterT[IO, String, ?]]

  def writerTMonadCatch = monadCatchTestsFor[WriterT[IO, Int, ?]]

  def monadIOTestsFor[F[_]](implicit FEI: Eq[F[Int]], FES: Eq[F[String]], FM: MonadIO[F]) =
    mkProp(monadIO.laws[F, Int, String])

  def monadCatchTestsFor[F[_]](implicit FA: Arbitrary[F[Int]], FE: Eq[F[Int]], FM: MonadCatch[F]) =
    mkProp(monadCatch.laws[F, Int])

  def mkProp(ps: Properties): Fragments =
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
