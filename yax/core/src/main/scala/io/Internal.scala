package io

#+cats
import cats.{Monad, Monoid}
import cats.data.{Kleisli, OptionT, StateT, WriterT, Xor => Either, XorT => EitherT}
import cats.std.either.{eitherInstances => seitherMonad}
#-cats

#+scalaz
import scalaz.{\/ => Either, EitherT, Kleisli, OptionT, Monad, Monoid, StateT, WriterT}
import scalaz.std.either.{eitherMonad => seitherMonad}
#-scalaz

import scala.util.{Either => SEither}

private[io] object monadInstances {
  def either[A]: Monad[Either[A, ?]] =
#+cats
    Either.xorInstances[A]
#-cats

#+scalaz
    Either.DisjunctionInstances1[A]
#-scalaz

  def eitherT[F[_]: Monad, A]: Monad[EitherT[F, A, ?]] =
#+cats
    EitherT.xorTMonadError[F, A]
#-cats

#+scalaz
    EitherT.eitherTMonad[F, A]
#-scalaz

  def seither[A]: Monad[SEither[A, ?]] = seitherMonad

  def kleisli[F[_]: Monad, A]: Monad[Kleisli[F, A, ?]] =
#+cats
    Kleisli.kleisliMonadReader[F, A]
#-cats

#+scalaz
    Kleisli.kleisliMonadReader[F, A]
#-scalaz

  def optionT[F[_]: Monad]: Monad[OptionT[F, ?]] =
#+cats
    OptionT.optionTMonad[F]
#-cats

#+scalaz
    OptionT.optionTMonadPlus[F]
#-scalaz

  def stateT[F[_]: Monad, A]: Monad[StateT[F, A, ?]] =
#+cats
    StateT.stateTMonadState[F, A]
#-cats

#+scalaz
    StateT.stateTMonadState[A, F]
#-scalaz

  def writerT[F[_]: Monad, A: Monoid]: Monad[WriterT[F, A, ?]] =
#+cats
    WriterT.writerTMonadWriter[F, A]
#-cats

#+scalaz
    WriterT.writerTMonadListen[F, A]
#-scalaz
}
