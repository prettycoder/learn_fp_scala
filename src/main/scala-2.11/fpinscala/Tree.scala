package fpinscala

/**
 * Created by maia.engeli on 2016-01-21.
 */
sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]): Int = {
    def loop(b: Tree[A], c: Int): Int =
      b match {
        case Branch(l, r) => loop(l, c+1) + loop(r, c+1)
        case Leaf(_) => c
      }
    loop(t, 0)
  }

  def apply

  end

  
}
