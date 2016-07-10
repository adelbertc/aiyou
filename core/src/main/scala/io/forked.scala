package io

import java.util.concurrent.{ Future => JFuture, Executors, ExecutorService, TimeUnit, ScheduledExecutorService, ScheduledFuture }

case class Forked[A](cancel: IO[Boolean], get: IO[A]) {

  def map[B](f: A => B): Forked[B] =
    Forked(cancel, get.map(f))

  def ap[B](ab: Forked[A => B]): Forked[B] =
    zip(ab).map { case (a, f) => f(a) }

  def zip[B](b: Forked[B]): Forked[(A, B)] =
    Forked(cancel.flatMap(a => b.cancel.map(a && _)), get.flatMap(a => b.get.map(b => (a, b))))

}

object Forked {

  def fromFuture[A](f: JFuture[A]): Forked[A] =
    Forked(IO.primitive(f.cancel(true)), IO.primitive(f.get))

  def pure[A](a: A): Forked[A] =
    Forked(IO.pure(false), IO.pure(a))

}

sealed trait SubmitIO {
  def submit[A](ioa: IO[A]): IO[Forked[A]]
  def shutdown: IO[Unit]
}

object SubmitIO {
  def fromExecutorService(e: ExecutorService) =
    new SubmitIO {
      def submit[A](io: IO[A]): IO[Forked[A]] =
        IO.primitive(e.submit(io.unsafeRunnable)).map(Forked.fromFuture)
      def shutdown: IO[Unit] =
        IO.primitive(e.shutdown)
    }
}

sealed trait ScheduleIO {
  def schedule[A](io: IO[A], delay: Long, units: TimeUnit): IO[Forked[A]]
}

object ScheduleIO {

  def fromScheduledExecutorService(e: ScheduledExecutorService) =
    new ScheduleIO {
      def schedule[A](io: IO[A], t: Long, tu: TimeUnit): IO[Forked[A]] =
        IO.primitive(e.schedule(io.unsafeRunnable, t, tu)).map { f =>
          Forked.fromFuture(f)
        }
    }

}

object ExecutorIO {

  def scheduledThreadPool(i: Int) =
    IO.primitive(Executors.newScheduledThreadPool(i)).map(ScheduleIO.fromScheduledExecutorService)

  def fixedThreadPool(i: Int): IO[SubmitIO] =
    IO.primitive(Executors.newFixedThreadPool(i)).map(SubmitIO.fromExecutorService)

  // def timeoutOn(e: ScheduleIO, t: Long, tu: TimeUnit) =
  //   for {
  //     x <- MVar.newEmptyMVar[Unit]
  //     _ <- e.schedule(x.put(()), t, tu)
  //     _ <- x.take
  //   } yield ()

  // def timeout(t: Long, tu: TimeUnit) =
  //   for {
  //     e <- scheduledThreadPool(1)
  //     _ <- timeoutOn(e, t, tu)
  //   } yield ()

  def forkIO[A](io: IO[A]): IO[Forked[A]] =
    fixedThreadPool(1).flatMap(_.submit(io))

  // def submitToMVar[A](io: IO[A], e: SubmitIO): IO[(MVar[A], Forked)] =
  //   for {
  //     m <- MVar.newEmptyMVar[A]
  //     f <- e.submit(io.flatMap(m.put(_)))
  //   } yield (m, f)

  // def waitAny[A](ios: NonEmptyList[IO[A]], e: SubmitIO): IO[A] =
  //   for {
  //     m <- MVar.newEmptyMVar[A]
  //     fs <- Traverse[NonEmptyList].traverse(ios) { io => e.submit(io.flatMap(m.put(_))) }
  //     a <- m.take
  //     _ <- Traverse[NonEmptyList].traverse_(fs)(_.cancel)
  //   } yield a

  // def waitBoth[A, B](ioa: IO[A], iob: IO[B], e: SubmitIO): IO[(A, B)] =
  //   for {
  //     af <- submitToMVar(ioa, e)
  //     bf <- submitToMVar(iob, e)
  //     a <- af._1.take
  //     b <- bf._1.take
  //   } yield (a, b)

  // def waitEither[A, B](ioa: IO[A], iob: IO[B], e: SubmitIO): IO[A \/ B] =
  //   waitAny(NonEmptyList(ioa.map(-\/(_)), iob.map(\/-(_))), e)
}