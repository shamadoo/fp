package fp

import scala.annotation.tailrec

/** Trait for a list.
  *
  * @tparam A   Type parameter of list elements
  */
sealed trait List[+A]

/** Empty list.
  */
case object Nil extends List[Nothing]

/** Class to append the head element to the tail of an existing list.
  *
  * @param  head  Head element to pre-pend
  * @param  tail  Existing tail of list
  * @tparam A     Type parameter of list elements
  */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

/** Companion object with List functions.
  */
object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  @tailrec
  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(x, xs) => append(xs, Cons(x, a2))
  }

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("empty list")
    case Cons(_, xs) => xs
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("empty list")
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => sys.error("empty list")
    case Cons(_, xs) => Cons(h, xs)
  }

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n == 0) l
    else l match {
      case Nil => Nil
      case Cons(_, xs) => drop(xs, n - 1)
    }
  }

  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => if (!f(x)) l else dropWhile(xs, f)
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def foldRightViaFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(as), z)((acc, x) => f(x, acc))
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, acc) => acc + 1)

  def map[A,B](l: List[A])(f: A => B): List[B] = foldRightViaFoldLeft(l, List[B]())((x, acc) => Cons(f(x), acc))

  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((acc, x) => Cons(x, acc))

  def concat[A](l: List[List[A]]): List[A] = foldRight(l, List[A]())(append)

  def filter[A](l: List[A])(f: A => Boolean): List[A] = {
    foldRightViaFoldLeft(l, List[A]())((x, acc) => if (f(x)) Cons(x, acc) else acc)
  }

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = concat(map(l)(f))

  def filterFromFlatMap[A](l: List[A])(f: A => Boolean): List[A] = flatMap(l)(x => if (f(x)) List(x) else Nil)

  def zipWith[A,B,C](a: List[A], b: List[B])(f: (A,B) => C): List[C] = (a,b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(f(h1,h2), zipWith(t1,t2)(f))
  }
}
