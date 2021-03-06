package fp

import scala.annotation.tailrec

/** Trait for a list.
  *
  * @tparam A   Type parameter of list elements
  */
sealed trait FPList[+A]

/** Empty list.
  */
case object Nil extends FPList[Nothing]

/** Class to append the head element to the tail of an existing list.
  *
  * @param  head  Head element to pre-pend
  * @param  tail  Existing tail of list
  * @tparam A     Type parameter of list elements
  */
case class Cons[+A](head: A, tail: FPList[A]) extends FPList[A]

/** Companion object with FPList functions.
  */
object FPList {

  def sum(ints: FPList[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: FPList[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): FPList[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  @tailrec
  def append[A](a1: FPList[A], a2: FPList[A]): FPList[A] = a1 match {
    case Nil => a2
    case Cons(x, xs) => append(xs, Cons(x, a2))
  }

  def tail[A](l: FPList[A]): FPList[A] = l match {
    case Nil => sys.error("empty list")
    case Cons(_, xs) => xs
  }

  def init[A](l: FPList[A]): FPList[A] = l match {
    case Nil => sys.error("empty list")
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def setHead[A](l: FPList[A], h: A): FPList[A] = l match {
    case Nil => sys.error("empty list")
    case Cons(_, xs) => Cons(h, xs)
  }

  @tailrec
  def drop[A](l: FPList[A], n: Int): FPList[A] = {
    if (n == 0) l
    else l match {
      case Nil => Nil
      case Cons(_, xs) => drop(xs, n - 1)
    }
  }

  @tailrec
  def dropWhile[A](l: FPList[A], f: A => Boolean): FPList[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => if (!f(x)) l else dropWhile(xs, f)
  }

  def foldRight[A,B](as: FPList[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  @annotation.tailrec
  def foldLeft[A,B](l: FPList[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def length[A](l: FPList[A]): Int = foldRight(l, 0)((_, acc) => acc + 1)

  def map[A,B](l: FPList[A])(f: A => B): FPList[B] = foldRightViaFoldLeft(l, FPList[B]())((x, acc) => Cons(f(x), acc))

  def reverse[A](l: FPList[A]): FPList[A] = foldLeft(l, FPList[A]())((acc, x) => Cons(x, acc))

  def concat[A](l: FPList[FPList[A]]): FPList[A] = foldRight(l, FPList[A]())(append)

  def filter[A](l: FPList[A])(f: A => Boolean): FPList[A] = {
    foldRightViaFoldLeft(l, FPList[A]())((x, acc) => if (f(x)) Cons(x, acc) else acc)
  }

  def flatMap[A,B](l: FPList[A])(f: A => FPList[B]): FPList[B] = concat(map(l)(f))

  def zipWith[A,B,C](a: FPList[A], b: FPList[B])(f: (A,B) => C): FPList[C] = (a,b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(f(h1,h2), zipWith(t1,t2)(f))
  }

  protected def filterFromFlatMap[A](l: FPList[A])(f: A => Boolean): FPList[A] = flatMap(l)(x => if (f(x)) FPList(x) else Nil)

  protected def foldRightViaFoldLeft[A, B](as: FPList[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(as), z)((acc, x) => f(x, acc))
  }
}
