import scala.annotation.tailrec

type Set = Int => Boolean

/**
  * Indicates whether a set contains a given element.
  */
def contains(s: Set, elem: Int): Boolean = s(elem)

/**
  * Returns the set of the one given element.
  */
def singletonSet(elem: Int): Set = x => x == elem

/**
  * Returns the union of the two given sets,
  * the sets of all elements that are in either `s` or `t`.
  */
def union(s: Set, t: Set): Set = x => s(x) | t(x)

def intersect(s: Set, t: Set): Set = x => s(x) & t(x)

def diff(s: Set, t: Set): Set = x => s(x) & !t(x)

def filter(s: Set, p: Int => Boolean): Set = x => p(x) & s(x)

val bound = 1000

/**
  * Returns whether all bounded integers within `s` satisfy `p`.
  */
def forall(s: Set, p: Int => Boolean): Boolean = {
  @tailrec
  def iter(a: Int): Boolean = {
    if (a > bound) true
    else if (s(a) && !p(a)) false
    else iter(a + 1)
  }

  iter(-bound)
}

def exists(s: Set, p: Int => Boolean): Boolean = {
  !forall(s, x => !p(x))
}

def map(s: Set, f: Int => Int): Set = {
  @tailrec
  def _map(result: Set, i: Int): Set = {
    if (i > bound) result
    else if (s(i)) {
      val next_result = if (result != Nil) result else singletonSet(f(i))
      _map(union(next_result, singletonSet(f(i))), i + 1)
    }
    else _map(result, i + 1)
  }

  _map(Nil, -bound)
}

def setToString(s: Set): String = {
  val xs = for (i <- -bound to bound if contains(s, i)) yield i
  xs.mkString("{", ",", "}")
}

/**
  * Prints the contents of a set on the console.
  */
def printSet(s: Set) {
  println(setToString(s))
}



val s1 = singletonSet(1)
val s2 = singletonSet(2)
val s3 = singletonSet(3)
val s4 = singletonSet(4)
val s5 = singletonSet(5)

val u1_2 = union(s1, s2)
val u1_2_3 = union(u1_2, s3)
val u1_2_3_4 = union(u1_2_3, s4)
val u1_2_3_4_5 = union(u1_2_3_4, s5)

contains(u1_2, 3)
contains(u1_2, 2)

//create an intersection of [1,2] and [1]==> [1]
println("intersection===============")
val i1_2 = intersect(union(s1, s2), s1)
contains(i1_2, 2) //false
contains(i1_2, 1) //true


//create an diff of [1,2] and [1]==> [2]
println("diff===============")
val d1_2 = diff(union(s1, s2), s1)
assert(contains(d1_2, 2), true) //true
contains(d1_2, 1) //false


println("filter===============")
contains(u1_2_3_4_5, 3)
val f_greater_3 = filter(u1_2_3_4_5, x => x > 3)
contains(f_greater_3, 2)
contains(f_greater_3, 5)

println("exists=============")
exists(f_greater_3, x => x == 4)

println("maps=================")
val m1_plus_3 = map(s1, x => x + 3)
contains(m1_plus_3, 3)
contains(m1_plus_3, 4)

println("printing===========")
printSet(map((x => x < -900), y => y * y))


