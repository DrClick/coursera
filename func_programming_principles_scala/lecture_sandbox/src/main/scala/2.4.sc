import math.abs

val tolerance = 0.00001

def isCloseEnough(x: Double, y: Double): Boolean =
  abs((x - y) / x) / x < tolerance


def fixedPoint(f: Double => Double)(firstGuess: Double): Double = {
  def iterate(guess: Double): Double = {
    val next = f(guess)
    if (isCloseEnough(next, guess)) next
    else iterate(next)
  }

  iterate(firstGuess)
}

//sqrt(x) is the number y such that y * y = x
//there fore it can be defined as the fixed point of y = x/y

//however, since sqrt can oscillate, we avg the last two values
//to find the fixed point of y = (y + x/y)/2
//  def sqrt(x: Double):Double = fixedPoint(y => (y + x/y)/2)(x/2)


def averageDamp(f: Double => Double)(x: Double): Double = (x + f(x)) / 2

def sqrt(x: Double): Double = fixedPoint(averageDamp(y => x / y))(x / 2)
