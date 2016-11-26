package aiyou

trait MonadCatchClass[F[_]] extends MonadCatch[F] with MonadThrowClass[F] {
  final def monadCatch: MonadCatch[F] = this
}
