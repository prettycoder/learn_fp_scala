package fpinscala_ch_1_4

sealed trait List2[+A]
case object Nil extends List2[Nothing]
case class Cons2[+A](head: A, tail: List2[A]) extends List2[A]

object List2 {
  def sum(ints: List2[Int]): Int = ints match {
    case Nil => 0
    case Cons2(x, xs) => x + sum(xs)
  }

  def product(ds: List2[Double]): Double = ds match {
    case Nil => 1.0
    case Cons2(0.0, _) => 0.0
    case Cons2(x, xs) => x * product(xs)
  }

  def tail[A](ls: List2[A]): List2[A] = ls match {
    case Nil => Nil
    case Cons2(x, xs) => xs
  }

  def head[Int](ls: List2[Int]): Int = ls match {
    // don't know how to return nil for type A or raise exception
    case Cons2(x, xs) => x
    case Nil => sys.error("get Head from empty list")
  }

  def setHead[A](head: A, ls: List2[A]): List2[A] = {
    Cons2(head, ls)
  }

  def drop[A](ls: List2[A], n: Int): List2[A] = {
    def loop(ls2: List2[A], m: Int): List2[A] =
      if(ls2 == Nil) Nil
      else if(m <= 0) ls2
      else loop(tail(ls2), m-1)

    loop(ls,n)
  }

  def dropWhile[A](ls: List2[A])(f: A => Boolean): List2[A] =
    ls match {
      case Cons2(h,t) if f(h) => dropWhile(t)(f)
      case _ => ls
  }

  def init[A](ls: List2[A]): List2[A] = {
    def reverse(ls2: List2[A], ls3: List2[A]): List2[A] = ls2 match {
      case Nil => ls3
      case Cons2(x, xs) => reverse(xs, Cons2(x, ls3))
    }
    reverse(tail(reverse(ls, Nil)), Nil)
  }

  def foldRight[A,B](ls: List2[A], z: B)(f: (A,B) => B): B =
    ls match {
      case Nil => z
      case Cons2(h,t) => f(h, foldRight(t, z)(f))
  }

  def foldLeft[A,B](ls: List2[A], z: B)(f: (B,A) => B): B =
    ls match {
      case Nil => z
      case Cons2(h,t) => foldLeft(t, f(z,h))(f)
  }

  def sum2(ls: List2[Int]) =
    foldRight(ls, 0)((x,y) => x+y)

  def sum3(ls: List2[Int]) =
    foldLeft(ls, 0)(_ + _)

  def product3(ls: List2[Double]) =
    foldLeft(ls, 1.0)(_ * _)

  def length3[A](ls: List2[A]) =
    foldLeft(ls, 0)((x,y) => x + 1)

  def reverse[A](ls: List2[A]) =
    foldLeft(ls, Nil:List2[A])((x,y) => Cons2(y,x))

  def append[A](ls1: List2[A], ls2: List2[A]) =
    foldLeft(reverse(ls1), ls2)((x,y) => Cons2(y,x))

  def concat[A](ls: List2[List2[A]]) =
    foldLeft(ls, Nil:List2[A])((x,y) => append(x,y))

  def product2(ls: List2[Double]) =
    foldRight(ls, 1.0)(_ * _)

  def length[A](ls: List2[A]): Int = {
    foldRight(ls, 0)((x,y) => 1 + y)
  }

  // 3.16
  def add_one(ls: List2[Int]): List2[Int] = {
    def loop(ls1: List2[Int], ls2: List2[Int]): List2[Int] =
      ls1 match {
        case Nil => ls2
        case Cons2(t, h) => loop(h, Cons2(t + 1, ls2))
      }

    reverse(loop(ls, Nil:List2[Int]))
  }

  // 3.17
  def as_strings(ls: List2[Double]): List2[String] = {
    def loop(ls1: List2[Double], ls2: List2[String]): List2[String] =
      ls1 match {
        case Nil => ls2
        case Cons2(h, t) => loop(t, Cons2(h.toString, ls2))
      }

    reverse(loop(ls, Nil:List2[String]))
  }

  // 3.18
  def map[A,B](as: List2[A])(f: A => B): List2[B] = {
    def loop(ls1: List2[A], ls2: List2[B]): List2[B] =
      ls1 match {
        case Nil => ls2
        case Cons2(h, t) => loop(t, Cons2(f(h), ls2))
      }

    reverse(loop(as, Nil:List2[B]))
  }

  // 3.19
  def filter[A](as: List2[A])(f: A => Boolean): List2[A] = {
    def loop(ls1: List2[A], ls2: List2[A]): List2[A] =
      ls1 match {
        case Nil => ls2
        case Cons2(h,t) =>
          if(f(h)) loop(t, Cons2(h, ls2))
          else loop(t, ls2)
      }

    reverse(loop(as, Nil:List2[A]))
  }

  // 3.20
  def flatMap[A,B](as: List2[A])(f: A => List2[B]): List2[B] = {
    def loop(ls1: List2[A], ls2: List2[B]): List2[B] =
      ls1 match {
        case Nil => ls2
        case Cons2(h, t) => loop(t, append(ls2, f(h)))
      }

    loop(as, Nil:List2[B])
  }

  // 3.21
  def filter2[A](as: List2[A])(f: A => Boolean): List2[A] = {
    List2.flatMap(as)((a: A) => if(f(a)) List2[A](a) else Nil:List2[A])
  }

  // 3.22
  def adds(as1: List2[Int], as2: List2[Int]): List2[Int] = {
    if(length3(as1) != length(as2)) return Nil:List2[Int]
    def loop(as1: List2[Int], as2: List2[Int], as3: List2[Int]): List2[Int] =
      as1 match {
        case Nil => as3
        case Cons2(h,t) => loop(t, tail(as2), Cons2(h + head(as2), as3))
      }

    reverse(loop(as1, as2, Nil:List2[Int]))
  }

  // 3.23
  def zipWith[A](as1: List2[A], as2: List2[A])(f: (A,A) => A): List2[A] = {
    if(length3(as1) != length(as2)) return Nil:List2[A]
    def loop(as1: List2[A], as2: List2[A], as3: List2[A]): List2[A] =
      as1 match {
        case Nil => as3
        case Cons2(h,t) => loop(t, tail(as2), Cons2(f(h,head(as2)), as3))
      }

    reverse(loop(as1, as2, Nil:List2[A]))
  }

  // 3.24
  def startsWith[A](ls1: List2[A], ls2: List2[A]): Boolean = (ls1,ls2) match {
    case (_,Nil) => true
    case (Cons2(h,t),Cons2(h2,t2)) if h == h2 => startsWith(t, t2)
    case _ => false
  }

  def hasSubsequence[A](as: List2[A], sub: List2[A]): Boolean = as match {
    case (Nil) => false
    case _ if startsWith(as, sub) => true
    case (Cons2(_,t)) => hasSubsequence(t, sub)
  }


  def apply[A](as: A*): List2[A] =
    if (as.isEmpty) Nil
    else Cons2(as.head, apply(as.tail: _*))
}
