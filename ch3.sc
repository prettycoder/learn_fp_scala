import fpinscala._

var a = List(1,2)
List.sum(a)

// Chapter 3 - Pattern Matching

// 3.1
val x = List(1,2,3,4,5) match {
  case Cons(x, Cons(2, Cons(4, _))) => x
  case Nil => 42
  case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
  case Cons(h, t) => h + List.sum(t)
  case _ => 101
}
// 3

// 3.2 define tail function
List.tail(List(1,2,3,4,5))

// 3.3
List.setHead(7, List(1,2,3,4,5))

// 3.4
List.drop(List(1,2,3,4,5,6,7,8,9), 3)

// 3.5
List.dropWhile(List(1,2,3,4,5,6,7))(_*2 < 7)

// 3.6
List.init(List(1,2,3,4,5,6,7,8))

// 3.7
// for product it could and for division it should, but not for other functions

// 3.8
List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))

// 3.9
List.length(List(1,2,3,4, 5, 6, 7))

// 3.10
List.foldLeft(List(1,5,10), 1.0)(_ / _)

// 3.11
List.sum3(List(1,2,3,4))
List.product3(List(1,2,3,4))
List.length3(List(1,2,3,4))

// 3.12
List.reverse(List(1,2,3))

// 3.13


// 3.14
List.append(List(1,2,3), List(4,5))

// 3.15
List.concat(List(List(1,2,3),List(4,5),List(6,7,8,9,10)))






