
class Rational(x:Int, y:Int){
  def numer = x
  def denom = y


  def add(other:Rational) =
    new Rational( ((numer*other.denom) + (denom*other.numer)), (denom*other.denom))

  override def  toString = numer + "/" + denom
  def printRational() = print (numer + "/" + denom)

}

object rationals{
  val r = new Rational(2,3)
  r.denom
  r.numer

  val y = new Rational(1,3)
  r.toString()
  r.add(y)
  r.toString()

  r.printRational()

}
