import scala.annotation.tailrec


def sum_non_tail_curried(f: Int => Int)(a: Int, b: Int): Int = {
  if (a > b) 0 else f(a) + sum(f)(a + 1, b)
}


//tail recursive version
def sum(f: Int => Int): (Int, Int) => Int = {
  @tailrec
  def _sum(a: Int, b: Int, acc: Int): Int = {
    if (a > b) acc else _sum(a + 1, b, acc + f(a))
  }

  _sum(_, _, 0)
}

//sums up all numbers between 1 and 5 => 15
sum(x => x)(1, 5)

//def product(f: Int => Int)(a: Int, b: Int): Int = {
//  if (a > b) 1 else f(a) * product(f)(a + 1, b)
//}


def mapReduce(map: Int => Int,
              combine: (Int, Int) => Int,
              zero: Int)(a: Int, b: Int): Int = {

  if (a > b) zero else {
    combine(map(a), mapReduce(map, combine, zero)(a + 1, b))
  }
}

def product(f: Int => Int)(a: Int, b: Int): Int =
  mapReduce(f, (a, b) => a * b, 1)(a, b)


def factorial(n: Int): Int = product(x => x)(1, n)


factorial(6)
