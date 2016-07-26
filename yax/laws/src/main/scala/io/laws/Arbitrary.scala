package io
package laws

import org.scalacheck.Arbitrary

object arbitrary {
  implicit def ioLawsArbitraryForIO[A: Arbitrary]: Arbitrary[IO[A]] =
    Arbitrary(Arbitrary.arbitrary[A].map(IO.pure))
}
