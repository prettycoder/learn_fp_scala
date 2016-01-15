package fpinscala

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

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

  def tail[A](ls: List[A]): List[A] = ls match {
    case Nil => Nil
    case Cons(x, xs) => xs
  }

  def setHead[A](head: A, ls: List[A]): List[A] = {
    Cons(head, ls)
  }

  def drop[A](ls: List[A], n: Int): List[A] = {
    def loop(ls2: List[A], m: Int): List[A] =
      if(ls2 == Nil) Nil
      else if(m <= 0) ls2
      else loop(tail(ls2), m-1)

    loop(ls,n)
  }

  def dropWhile[A](ls: List[A])(f: A => Boolean): List[A] =
    ls match {
      case Cons(h,t) if f(h) => dropWhile(t)(f)
      case _ => ls
  }

  def init[A](ls: List[A]): List[A] = {
    def reverse(ls2: List[A], ls3: List[A]): List[A] = ls2 match {
      case Nil => ls3
      case Cons(x, xs) => reverse(xs, Cons(x, ls3))
    }
    reverse(tail(reverse(ls, Nil)), Nil)
  }

  def foldRight[A,B](ls: List[A], z: B)(f: (A,B) => B): B =
    ls match {
      case Nil => z
      case Cons(h,t) => f(h, foldRight(t, z)(f))
  }

  def foldLeft[A,B](ls: List[A], z: B)(f: (B,A) => B): B =
    ls match {
      case Nil => z
      case Cons(h,t) => foldLeft(t, f(z,h))(f)
  }

  def sum2(ls: List[Int]) =
    foldRight(ls, 0)((x,y) => x+y)

  def sum3(ls: List[Int]) =
    foldLeft(ls, 0)(_ + _)

  def product3(ls: List[Double]) =
    foldLeft(ls, 1.0)(_ * _)

  def length3[A](ls: List[A]) =
    foldLeft(ls, 0)((x,y) => x + 1)

  def reverse[A](ls: List[A]) =
    foldLeft(ls, Nil:List[A])((x,y) => Cons(y,x))

  def append[A](ls1: List[A], ls2: List[A]) =
    foldLeft(reverse(ls1), ls2)((x,y) => Cons(y,x))

  def concat[A](ls: List[List[A]]) =
    foldLeft(ls, Nil:List[A])((x,y) => append(x,y))

  def product2(ls: List[Double]) =
    foldRight(ls, 1.0)(_ * _)

  def length[A](ls: List[A]): Int = {
    foldRight(ls, 0)((x,y) => 1 + y)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}

object myApp extends App {
  println(List.sum(List(1,2,3,4)))
}

