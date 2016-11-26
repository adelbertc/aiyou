package aiyou

/** Safe way of running a program.
  *
  * Must override either `run` (no command line arguments) or `runl`.
  */
trait SafeApp {
  def runl(args: List[String]): IO[Unit] = run

  def run: IO[Unit] = IO.unit

  final def main(args: Array[String]): Unit = runl(args.toList).unsafePerformIO()
}
