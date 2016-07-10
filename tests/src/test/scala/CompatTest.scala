import io._

// compilation tests to verify that inference works for compat layers
object CompatTest {

  object ScalazCompat {
  }

  object StdlibCompat {
    import io.compat.stdlib._

    val a: IO[Int] = IO.pure(1)

    val b  = a.attempt
    val b0 = b : IO[Either[Throwable, Int]]

    val c  = a.attemptSome { case t: Throwable => "c" }
    val c0 = c : IO[Either[String, Int]]
    
  }

  object CatsCompat {
  }

}
