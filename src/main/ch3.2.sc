import fpinscala._
1+1


// Chapter 3 - Pattern Matching

// 3.16
List.add_one(List(1,2,4))

// 3.17
List.as_strings(List(1,2,3,4.5))

// 3.18
List.map(List(2,3,4))(_ + 2)

// 3.19
List.filter(List(2,3,4))(_ >= 3)

// 3.20
List.flatMap(List(6,2,3))(i => List(i,i))

// 3.21
List.filter2(List(2,3,4,5,6,7))(_ > 3)

// 3.22
List.adds(List(2,3,4,5,6), List(5,5,5,5,200))

// 3.23
List.zipWith(List(2,3,4), List(2,3,4))(_ * _)

// 3.24

// 3.25
// Tree.size(Tree(1, Tree(1, Tree(1,)))))
//Tree(1)
//Tree(1,1)
Tree(Tree(1), Tree(1))

