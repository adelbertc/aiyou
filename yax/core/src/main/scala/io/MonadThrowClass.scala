package aiyou

trait MonadThrowClass[F[_]] extends MonadThrow[F] {
  final def monadThrow: MonadThrow[F] = this
}
