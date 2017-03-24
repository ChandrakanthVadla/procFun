
type Set = Int => Boolean
def contains(s: Set, elem: Int): Boolean = s(elem)
def singletonSet(elem: Int): Set = (x : Int) => x == elem
def union(s: Set, t: Set): Set = (x:Int) => s(x) || t(x)
def intersect(s: Set, t: Set): Set = (x : Int) => s(x) && t(x)
def diff(s: Set, t: Set): Set = (x : Int) => s(x) && ! t(x)
def filter(s: Set, p: Int => Boolean): Set = (x : Int) => s(x) &&p(x)
  //if(contains(s,x)) p(x) else false

val bound = 10




val s1 = singletonSet(1)
val s2 = singletonSet(2)
val s3 = singletonSet(2)
val s4 = singletonSet(3)
val s5 = singletonSet(5)


val s12 = union(s1,s2)
val s34 = union(s3,s4)

val s1234 = union(s12,s34)

def forall(s: Set, p: Int => Boolean): Boolean = {
  def iter(a: Int): Boolean = {
    if (a >= bound)  true
    else if(contains(s,a) && ! p(a)) false
    else iter(a + 1)
  }
  iter(-bound)
}
def exists(s: Set, p: Int => Boolean): Boolean = !forall(s, x => !p(x))
def map(s: Set, f: Int => Int): Set = (x: Int) => exists(s, y=> f(y)==x)



exists(s5, (x =>  x==4))
forall(s5, (x => x==4) )
val s6 = map(s5,x=>x+1)
contains(s6,6)
