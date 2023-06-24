package homework.hw5

import scala.language.implicitConversions

object hkt extends App {

  implicit def ba: List[Int] => Bindable[List, Int] = list => {
    new Bindable[List, Int] {
      override def map[B](f: Int => B): List[B] = list.map(f)

      override def flatMap[B](f: Int => List[B]): List[B] = list.flatMap(f)
    }
  }

  implicit def bb: List[String] => Bindable[List, String] = list => {
    new Bindable[List, String] {
      override def map[B](f: String => B): List[B] = list.map(f)

      override def flatMap[B](f: String => List[B]): List[B] = list.flatMap(f)
    }
  }

  trait Bindable[F[_], A] {
    def map[B](f: A => B): F[B]

    def flatMap[B](f: A => F[B]): F[B]
  }

  def tuplef[F[_], A, B](fa: F[A], fb: F[B])(implicit ba: F[A] => Bindable[F, A], bb: F[B] => Bindable[F, B]) : F[(A, B)] = {
    ba(fa).flatMap(f => bb(fb).map((f, _)))
  }

}
