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

  def head[A](ls: List[A]): A = ls match {
    // don't know how to return nil for type A or raise exception
    case Cons(x, xs) => x
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

  // 3.16
  def add_one(ls: List[Int]): List[Int] = {
    def loop(ls1: List[Int], ls2: List[Int]): List[Int] =
      ls1 match {
        case Nil => ls2
        case Cons(t, h) => loop(h, Cons(t + 1, ls2))
      }

    reverse(loop(ls, Nil:List[Int]))
  }

  // 3.17
  def as_strings(ls: List[Double]): List[String] = {
    def loop(ls1: List[Double], ls2: List[String]): List[String] =
      ls1 match {
        case Nil => ls2
        case Cons(h, t) => loop(t, Cons(h.toString, ls2))
      }

    reverse(loop(ls, Nil:List[String]))
  }

  // 3.18
  def map[A,B](as: List[A])(f: A => B): List[B] = {
    def loop(ls1: List[A], ls2: List[B]): List[B] =
      ls1 match {
        case Nil => ls2
        case Cons(h, t) => loop(t, Cons(f(h), ls2))
      }

    reverse(loop(as, Nil:List[B]))
  }

  // 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    def loop(ls1: List[A], ls2: List[A]): List[A] =
      ls1 match {
        case Nil => ls2
        case Cons(h,t) =>
          if(f(h)) loop(t, Cons(h, ls2))
          else loop(t, ls2)
      }

    reverse(loop(as, Nil:List[A]))
  }

  // 3.20
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
    def loop(ls1: List[A], ls2: List[B]): List[B] =
      ls1 match {
        case Nil => ls2
        case Cons(h, t) => loop(t, append(ls2, f(h)))
      }

    loop(as, Nil:List[B])
  }

  // 3.21
  def filter2[A](as: List[A])(f: A => Boolean): List[A] = {
    List.flatMap(as)((a: A) => if(f(a)) List[A](a) else Nil:List[A])
  }

  // 3.22
  def adds(as1: List[Int], as2: List[Int]): List[Int] = {
    if(length3(as1) != length(as2)) return Nil:List[Int]
    def loop(as1: List[Int], as2: List[Int], as3: List[Int]): List[Int] =
      as1 match {
        case Nil => as3
        case Cons(h,t) => loop(t, tail(as2), Cons(h + head(as2), as3))
      }

    reverse(loop(as1, as2, Nil:List[Int]))
  }

  // 3.23
  def zipWith[A](as1: List[A], as2: List[A])(f: (A,A) => A): List[A] = {
    if(length3(as1) != length(as2)) return Nil:List[A]
    def loop(as1: List[A], as2: List[A], as3: List[A]): List[A] =
      as1 match {
        case Nil => as3
        case Cons(h,t) => loop(t, tail(as2), Cons(f(h,head(as2)), as3))
      }

    reverse(loop(as1, as2, Nil:List[A]))
  }

  // 3.24
  def hasSubsequence[A](as: List[A], sub: List[A]): Boolean = {
    true
  }


  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}
