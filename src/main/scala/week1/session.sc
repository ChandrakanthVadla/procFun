def factorial (n:Int) : Int = {
  def loop(acc:Int, n:Int):Int =
    if(n==0) acc
    else loop(acc*n, n-1)
  loop(1, n)
}
factorial(5)

//sum function using tail recurssion
def sum(f: Int => Int)(a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int = {
    //print ("entred the loop with ",a," and ", acc, "/n")
    if (a > b) acc
    else loop(a+1, acc+f(a))
  }
  loop(a, 0)
}

sum (x => x) (2,5)
sum (x => x*x ) (2,5)


def mapReduce(f:Int => Int, combine:(Int, Int) => Int, acc:Int)(a:Int, b:Int):Int =
  if (a>b) acc
  else combine(f(a), mapReduce(f,combine, acc) (a+1,b))

def product(f:Int => Int )(a:Int, b:Int) = mapReduce(f, (x,y) => x*y , 1)(a,b)

//product of numbers in range a,b
product(x=> x) (2,3) //squar(2) * square(2) = 4*9 =

//product of square of numbers in range
product(x=> x*x) (2,3) //squar(2) * square(2) = 4*9 =

def SumOpRange(f:Int =>Int)(a:Int, b:Int) = mapReduce(f , (x,y)=>x+y, 0) (a,b)

SumOpRange(x=>x)(2,3)   //sum of numbers in range a,b
SumOpRange(x=>x*x)(2,3) //sum of squares of numbers in range a,b

def fact(n:Int) = product(x => x)(1,n)
fact(6)