package fpinscala_ch_1_4


sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

// 3.25
  def size[A](t: Tree[A]): Int =
    t match {
      case Branch(l,r) => 1 + size(l) + size(r)
      case Leaf(_) => 1
    }

// 3.26
  def maximum(t: Tree[Int]): Int =
    t match {
      case Branch(l,r) => maximum(l) max maximum(r)
      case Leaf(n) => n
    }

// 3.27
  def depth[A](t: Tree[A]): Int = {
    t match {
      case Branch(l,r) => (1+depth(l)) max (1+depth(r))
      case Leaf(_) => 1
    }
  }

// 3.28
  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = {
    t match {
      case Branch(l,r) => Branch(map(l)(f), map(r)(f))
      case Leaf(n) => Leaf(f(n))
    }
  }

// 3.29
  def fold[A](t: Tree[A])(g: A => Int)(f: (Int, Int) => Int): Int = {
    t match {
      case Branch(l,r) => f(fold(l)(g)(f), fold(r)(g)(f))
      case Leaf(n) => g(n)
    }
  }

}
