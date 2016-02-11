package fpinscala_ch5

//sealed trait Stream[+A]
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

sealed trait Stream[+A] {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }
  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty)
      empty
    else
      cons(as.head, apply(as.tail: _*))

  // 5.1 convert Stream to List2
  def toList0: List[A] =
    this match {
      case Cons(h,t) => h() :: t().toList0
      case _ => List()
    }

  def toList: List[A] = {
      def go(s: Stream[A], acc: List[A]): List[A] = s match {
        case Cons(h,t) => go(t(), h() :: acc)
        case _ => acc
      }
      go(this, List()).reverse
    }

  // 5.2
  def take(n: Int): Stream[A] =
    this match {
      case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
      case Cons(h, _) if n == 1 => cons(h(), empty)
      case _ => empty
    }

  final def drop(n: Int): Stream[A] =
    this match {
      case Cons(_,t) if n > 0 => t().drop(n-1)
      case _ => this
    }

  // 5.3
  def takeWhile(p: A => Boolean): Stream[A] =
    this match {
      case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
      case _ => empty
    }

  // Book example

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  // 5.4
  def forAll(p: A => Boolean): Boolean =
    this match {
      case Cons(h,t) => p(h()) && t().forAll(p)
      case _ => false
    }

  // 5.5
  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h,t) =>
      if(p(h)) cons(h,t)
      else     empty )


}
