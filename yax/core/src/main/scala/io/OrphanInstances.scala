package aiyou

#+cats
import cats.{Monad, MonadError, Monoid}
import cats.data.{EitherT, Kleisli, OptionT, StateT, WriterT}
import cats.implicits._
#-cats

#+scalaz
import scalaz.{\/, -\/ => Left, \/- => Right, EitherT, Kleisli, Monad, MonadError, Monoid, OptionT, StateT, WriterT}
import scalaz.Scalaz._
#-scalaz

private[aiyou] trait OrphanInstances {
  //////////////////////
  // Either instances //
  //////////////////////

  implicit val ioMonadCatchForEither: MonadCatch[Either[Throwable, ?]] =
    new MonadCatchClass[Either[Throwable, ?]] {
      val monadError = MonadError[Either[Throwable, ?], Throwable]
      val monad = Monad[Either[Throwable, ?]]
      def throwM[A](e: Throwable): Either[Throwable, A] = scala.util.Left(e)
      def catchM[A](fa: Either[Throwable, A])(handler: Throwable => Either[Throwable, A]): Either[Throwable, A] =
#+cats
        monadError.handleErrorWith(fa)(handler)
#-cats

#+scalaz
        monadError.handleError(fa)(handler)
#-scalaz
    }

#+scalaz
  implicit val ioMonadCatchForDisjunction: MonadCatch[\/[Throwable, ?]] =
    new MonadCatchClass[\/[Throwable, ?]] {
      val monad = Monad[\/[Throwable, ?]]
      def throwM[A](e: Throwable): \/[Throwable, A] = Left(e)
      def catchM[A](fa: \/[Throwable, A])(handler: Throwable => \/[Throwable, A]): \/[Throwable, A] =
        fa.recoverWith[Throwable, A] { case t => handler(t) }
    }
#-scalaz

  ///////////////////////
  // EitherT instances //
  ///////////////////////

  implicit def ioMonadIOForEitherT[F[_], X](implicit F0: MonadIO[F]): MonadIO[EitherT[F, X, ?]] =
    new MonadIOClass[EitherT[F, X, ?]] {
      implicit val F1: Monad[F] = F0.monad
      val monad = Monad[EitherT[F, X, ?]]

      def liftIO[A](io: IO[A]): EitherT[F, X, A] = EitherT(F0.liftIO(io.map(Right(_))))
    }

  implicit def ioMonadThrowForEitherT[F[_], X](implicit F0: MonadThrow[F]): MonadThrow[EitherT[F, X, ?]] =
    new EitherTMonadThrow[F, X] { val monadThrowF = F0 }

  implicit def ioMonadCatchForEitherT[F[_], X](implicit F0: MonadCatch[F]): MonadCatch[EitherT[F, X, ?]] =
    new EitherTMonadCatch[F, X] { val monadCatchF = F0 }

  ///////////////////////
  // Kleisli instances //
  ///////////////////////

  implicit def ioMonadIOForKleisli[F[_], X](implicit F0: MonadIO[F]): MonadIO[Kleisli[F, X, ?]] =
    new MonadIOClass[Kleisli[F, X, ?]] {
      implicit val F1: Monad[F] = MonadIO[F].monad
      val monad = Monad[Kleisli[F, X, ?]]

      def liftIO[A](io: IO[A]): Kleisli[F, X, A] = Kleisli(_ => F0.liftIO(io))
    }

  implicit def ioMonadThrowForKleisli[F[_], X](implicit F0: MonadThrow[F]): MonadThrow[Kleisli[F, X, ?]] =
    new KleisliMonadThrow[F, X] { val monadThrowF = F0 }

  implicit def ioMonadCatchForKleisli[F[_], X](implicit F0: MonadCatch[F]): MonadCatch[Kleisli[F, X, ?]] =
    new KleisliMonadCatch[F, X] { val monadCatchF = F0 }

  ///////////////////////
  // OptionT instances //
  ///////////////////////

  implicit def ioMonadIOForOptionT[F[_]](implicit F0: MonadIO[F]): MonadIO[OptionT[F, ?]] =
    new MonadIOClass[OptionT[F, ?]] {
      implicit val F1: Monad[F] = F0.monad
      val monad = Monad[OptionT[F, ?]]

      def liftIO[A](io: IO[A]): OptionT[F, A] = OptionT(F0.liftIO(io.map(Some(_))))
    }

  implicit def ioMonadThrowForOptionT[F[_]](implicit F0: MonadThrow[F]): MonadThrow[OptionT[F, ?]] =
    new OptionTMonadThrow[F] { val monadThrowF = F0 }

  implicit def ioMonadCatchForOptionT[F[_]](implicit F0: MonadCatch[F]): MonadCatch[OptionT[F, ?]] =
    new OptionTMonadCatch[F] { val monadCatchF = F0 }

  //////////////////////
  // StateT instances //
  //////////////////////

  implicit def ioMonadIOForStateT[F[_], X](implicit F0: MonadIO[F]): MonadIO[StateT[F, X, ?]] =
    new MonadIOClass[StateT[F, X, ?]] {
      implicit val F1: Monad[F] = F0.monad
      val monad = Monad[StateT[F, X, ?]]

      def liftIO[A](io: IO[A]): StateT[F, X, A] = StateT(state => F0.liftIO(io.map(a => (state, a))))
    }

  implicit def ioMonadThrowForStateT[F[_], X](implicit F0: MonadThrow[F]): MonadThrow[StateT[F, X, ?]] =
    new StateTMonadThrow[F, X] { val monadThrowF = F0 }

  implicit def ioMonadCatchForStateT[F[_], X](implicit F0: MonadCatch[F]): MonadCatch[StateT[F, X, ?]] =
    new StateTMonadCatch[F, X] { val monadCatchF = F0 }

  ///////////////////////
  // WriterT instances //
  ///////////////////////

  implicit def ioMonadIOForWriterT[F[_], X](implicit F0: MonadIO[F], X: Monoid[X]): MonadIO[WriterT[F, X, ?]] =
    new MonadIOClass[WriterT[F, X, ?]] {
#+cats
      val emptyX = X.empty
#-cats

#+scalaz
      val emptyX = X.zero
#-scalaz
      implicit val F1: Monad[F] = MonadIO[F].monad
      val monad = Monad[WriterT[F, X, ?]]

      def liftIO[A](io: IO[A]): WriterT[F, X, A] = WriterT(F0.liftIO(io.map(a => (emptyX, a))))
    }

  implicit def ioMonadThrowForWriterT[F[_], X](implicit F0: MonadThrow[F], X: Monoid[X]): MonadThrow[WriterT[F, X, ?]] =
    new WriterTMonadThrow[F, X] {
      val monadThrowF = F0
      val monoidX = X
    }

  implicit def ioMonadCatchForWriterT[F[_], X](implicit F0: MonadCatch[F], X: Monoid[X]): MonadCatch[WriterT[F, X, ?]] =
    new WriterTMonadCatch[F, X] {
      val monadCatchF = F0
      val monoidX = X
    }
}

/////////////////////
// Template traits //
/////////////////////

private[aiyou] trait EitherTMonadThrow[F[_], X] extends MonadThrowInductive[EitherT[?[_], X, ?], F] {
  def monad = Monad[EitherT[F, X, ?]]
  def throwM[A](e: Throwable): EitherT[F, X, A] = EitherT.left(monadThrowF.throwM(e))
}

private[aiyou] trait EitherTMonadCatch[F[_], X] extends MonadCatchInductive[EitherT[?[_], X, ?], F] with EitherTMonadThrow[F, X] {
  def catchM[A](fa: EitherT[F, X, A])(handler: Throwable => EitherT[F, X, A]): EitherT[F, X, A] = {
#+cats
    def unwrap[G[_], Y, Z](eithert: EitherT[G, Y, Z]): G[Either[Y, Z]] =
      eithert.value
#-cats

#+scalaz
    def unwrap[G[_], Y, Z](eithert: EitherT[G, Y, Z]): G[\/[Y, Z]] =
      eithert.run
#-scalaz

    EitherT(monadCatchF.catchM(unwrap(fa))(t => unwrap(handler(t))))
  }
}

private[aiyou] trait KleisliMonadThrow[F[_], X] extends MonadThrowInductive[Kleisli[?[_], X, ?], F] {
  def monad = Monad[Kleisli[F, X, ?]]
  def throwM[A](e: Throwable): Kleisli[F, X, A] = Kleisli(_ => monadThrowF.throwM(e))
}

private[aiyou] trait KleisliMonadCatch[F[_], X] extends MonadCatchInductive[Kleisli[?[_], X, ?], F] with KleisliMonadThrow[F, X] {
  def catchM[A](fa: Kleisli[F, X, A])(handler: Throwable => Kleisli[F, X, A]): Kleisli[F, X, A] =
    Kleisli((x: X) => monadCatchF.catchM(fa.run(x))(t => handler(t).run(x)))
}

private[aiyou] trait OptionTMonadThrow[F[_]] extends MonadThrowInductive[OptionT, F] {
  def monad = Monad[OptionT[F, ?]]
  def throwM[A](e: Throwable): OptionT[F, A] = OptionT(monadThrowF.throwM(e))
}

private[aiyou] trait OptionTMonadCatch[F[_]] extends MonadCatchInductive[OptionT, F] with OptionTMonadThrow[F] {
  def catchM[A](fa: OptionT[F, A])(handler: Throwable => OptionT[F, A]): OptionT[F, A] = {
    def unwrap[G[_], Y](optiont: OptionT[G, Y]): G[Option[Y]] =
#+cats
      optiont.value
#-cats

#+scalaz
      optiont.run
#-scalaz
    OptionT(monadCatchF.catchM(unwrap(fa))(t => unwrap(handler(t))))
  }
}

private[aiyou] trait StateTMonadThrow[F[_], X] extends MonadThrowInductive[StateT[?[_], X, ?], F] {
  def monad = Monad[StateT[F, X, ?]]
  def throwM[A](e: Throwable): StateT[F, X, A] = StateT(_ => monadThrowF.throwM(e))
}

private[aiyou] trait StateTMonadCatch[F[_], X] extends MonadCatchInductive[StateT[?[_], X, ?], F] with StateTMonadThrow[F, X] {
  def catchM[A](fa: StateT[F, X, A])(handler: Throwable => StateT[F, X, A]): StateT[F, X, A] =
    StateT(x => monadCatchF.catchM(fa.run(x))(t => handler(t).run(x)))
}

private[aiyou] trait WriterTMonadThrow[F[_], X] extends MonadThrowInductive[WriterT[?[_], X, ?], F] {
  implicit def monoidX: Monoid[X]
  def monad = Monad[WriterT[F, X, ?]]
  def throwM[A](e: Throwable): WriterT[F, X, A] = WriterT(monadThrowF.throwM(e))
}

private[aiyou] trait WriterTMonadCatch[F[_], X] extends MonadCatchInductive[WriterT[?[_], X, ?], F] with WriterTMonadThrow[F, X] {
  def catchM[A](fa: WriterT[F, X, A])(handler: Throwable => WriterT[F, X, A]): WriterT[F, X, A] =
    WriterT(monadCatchF.catchM(fa.run)(t => handler(t).run))
}

private[aiyou] trait MonadThrowInductive[MT[_[_], _], F[_]] extends MonadThrowClass[MT[F, ?]] {
  def monadThrowF: MonadThrow[F]
  implicit def monadF: Monad[F] = monadThrowF.monad
}

private[aiyou] trait MonadCatchInductive[MT[_[_], _], F[_]] extends MonadCatchClass[MT[F, ?]] with MonadThrowInductive[MT, F] {
  def monadCatchF: MonadCatch[F]
  def monadThrowF = monadCatchF.monadThrow
}
