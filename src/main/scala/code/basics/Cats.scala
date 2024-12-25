package code.basics

import cats.implicits.toFlatMapOps

object Cats {

  // Functor

  trait MyFunctor[F[_]] {
    def map[A, B](value: F[A])(f: A => B): F[B]
  }

  import cats.Functor
  import cats.instances.list.*

  // this is a functor instance for a List and provide map for Lists
  val listFunctor = Functor[List]

  // to generalize the API for a method link increment we can use the following API:
  def increment[F[_]](container: F[Int])(using functor: Functor[F]): F[Int] =
    functor.map(container)(_ + 1)

  import cats.syntax.functor.*

  def increment_v2[F[_]](container: F[Int])(using functor: Functor[F]): F[Int] =
    container.map(_ + 1)


  // Applicative

  trait MyApplicative[F[_]] {
    def pure[A](value: A): F[A]
  }

  import cats.Applicative

  // provide an applicative of type List which can lift any value into List
  val applicativeList = Applicative[List]
  val myList = applicativeList.pure(1) // 1 to List(1)

  import cats.syntax.applicative.*

  val myList2: Seq[Int] = 1.pure[List] // use Cats syntax to use pure directly on Int



  // FlatMap

  trait MyFlatMap[F[_]] extends MyFunctor[F] {
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  }


  import cats.FlatMap

  val flatMapList = FlatMap[List]
  val flatMappedList = 1.pure[List].flatMap(i => Seq(i + 1))

  // having map and flatmap on a type we can mix them like:
  // : FlatMap mean that F must be of FlatMap type which can be fulfilled by an implicit value in scope
  def crossProduct[F[_] : FlatMap, A, B, C](fa: F[A], fb: F[B])(g: (A, B) => C): F[C] =
    fa.flatMap(a => fb.map(b => g(a, b)))

  // having map and flatmap we can use for comprehension:
  def crossProduct_v2[F[_] : FlatMap, A, B, C](fa: F[A], fb: F[B])(g: (A, B) => C): F[C] =
    for {
      a <- fa
      b <- fb
    } yield g(a, b)



  // Monad = Applicative + FaltMap

  trait MyMonad[F[_]] extends MyApplicative[F] with MyFlatMap[F] {
    def map[A, B](fa: F[A])(g: A => B): F[B] =
      flatMap(fa)(a => pure(g(a)))
  }

  import cats.Monad

  val myMonad = Monad[List]

  def crossProduct_v3[F[_] : Monad, A, B, C](fa: F[A], fb: F[B])(g: (A, B) => C): F[C] =
    for {
      a <- fa
      b <- fb
    } yield g(a, b)


  // ApplicativeError -> run and store the error that can later be dealt with

  trait MyApplicativeError[F[_], E] extends MyApplicative[F] {
    def raiseError[A](e: E): F[A]
  }

  import cats.ApplicativeError

  type ErrorOr[A] = Either[String, A]
  val applicativeEither = ApplicativeError[ErrorOr, String]
  val errorOr1: ErrorOr[Int] = applicativeEither.pure(11)
  val errorOr2: ErrorOr[Int] = applicativeEither.raiseError("error happened!")


  // MonadError

  trait MyMonadError[F[_], E] extends MyApplicativeError[F, E] with MyMonad[F]


}
