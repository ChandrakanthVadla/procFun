import math.abs

object excercise{
  val tolarence = 0.0001
  def isCloseEnough(x:Double, y:Double) =
    abs((x-y)/x) < tolarence

  def fixedPoint(f:Double => Double) (firstGuess:Double) = {
    def iterate(guess:Double): Double = {
      val next = f(guess)
      if(isCloseEnough(guess, next)) next
      else iterate(next)
    }

    iterate(firstGuess)
  }

  def avgDamp (f:Double => Double) (x:Double) = (x+f(x))/2

  def sqrt(x:Double) = fixedPoint(avgDamp(y=>x/y))(1.0)

  sqrt(2)



}