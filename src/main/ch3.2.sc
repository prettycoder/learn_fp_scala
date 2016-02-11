import fpinscala_ch_1_4._

// Chapter 3 - Pattern Matching

// 3.16
List2.add_one(List2(1,2,4))

// 3.17
List2.as_strings(List2(1,2,3,4.5))

// 3.18
List2.map(List2(2,3,4))(_ + 2)

// 3.19
List2.filter(List2(2,3,4))(_ >= 3)

// 3.20
List2.flatMap(List2(6,2,3))(i => List2(i,i))

// 3.21
List2.filter2(List2(2,3,4,5,6,7))(_ > 3)

// 3.22
List2.adds(List2(2,3,4,5,6), List2(5,5,5,5,200))

// 3.23
List2.zipWith(List2(2,3,4), List2(2,3,4))(_ * _)

// 3.24

// 3.25
val t: Tree[String] = Branch(Leaf("hello"), Branch(Branch(Leaf(" World"),Leaf("!")), Leaf("!")))
Tree.size(t)

// 3.26
val t1: Tree[Int] = Branch(Leaf(7),
                      Branch(
                        Branch(
                          Branch(
                            Branch(Leaf(14),Leaf(2)),
                               Leaf(3)), Leaf(2)), Leaf(3)))
Tree.maximum(t1)

// 3.27
Tree.depth(t1)

// 3.28
Tree.map(t1)(_ + 1)

// 3.29
Tree.fold(t)(a => 1)(1 + _ + _)
Tree.fold(t1)(a => a)(_ max _)
Tree.fold(t1)(a => 1)((b,c) => (1 + b) max (1 + c))

