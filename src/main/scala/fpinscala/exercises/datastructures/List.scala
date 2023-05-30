package fpinscala.exercises.datastructures

/** `List` data type, parameterized on a type, `A`. */
enum List[+A]:
  /** A `List` data constructor representing the empty list. */
  case Nil
  /** Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
    which may be `Nil` or another `Cons`.
   */
  case Cons(head: A, tail: List[A])

object List: // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.

  def product(doubles: List[Double]): Double = doubles match
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  @annotation.nowarn // Scala gives a hint here via a warning, so let's disable that
  val result = List(1,2,3,4,5) match
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))

  def foldRight[A,B](as: List[A], acc: B, f: (A, B) => B): B = // Utility functions
    as match
      case Nil => acc
      case Cons(x, xs) => f(x, foldRight(xs, acc, f))

  def sumViaFoldRight(ns: List[Int]): Int =
    foldRight(ns, 0, (x,y) => x + y)

  def productViaFoldRight(ns: List[Double]): Double =
    foldRight(ns, 1.0, _ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] = 
    l match
      case Nil => sys.error("setHead on empty list")
      case Cons(head, tail) => tail

  def setHead[A](l: List[A], h: A): List[A] = 
    l match
      case Nil => sys.error("setHead on empty list")
      case Cons(head, tail) => Cons(h, tail)
    
  def drop[A](l: List[A], n: Int): List[A] = 
    if n <= 0 then l
    else l match
      case Nil => Nil
      case Cons(head, tail) => drop(tail, n-1)

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = 
    l match
      case Nil => Nil
      case Cons(head, tail) => 
        if f(head) then dropWhile(tail, f)
        else l

  def init[A](l: List[A]): List[A] = 
    l match
      case Nil => sys.error(".")
      case Cons(head, Nil) => Nil
      case Cons(head, tail) => Cons(head, init(tail))

  def length[A](l: List[A]): Int = 
    l match
      case Nil => 0
      case Cons(_, tail) => 1 + length(tail)

  def foldLeft[A,B](l: List[A], acc: B, f: (B, A) => B): B = 
    l match
      case Nil => acc
      case Cons(head, tail) => foldLeft(tail, f(acc, head), f)

  def sumViaFoldLeft(ns: List[Int]): Int = foldLeft(ns, 0, _ + _)

  def productViaFoldLeft(ns: List[Double]): Double = foldLeft(ns, 1.0, _ * _)

  def lengthViaFoldLeft[A](l: List[A]): Int = 
    foldLeft(l, 0, (acc, _) => acc + 1)

  def reverse[A](l: List[A]): List[A] = 
    foldLeft(l, Nil: List[A], (l, h) => Cons(h, l))

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] = 
    // foldRight(l, r, (h, r) => Cons(h, r))
    foldRight(l, r, Cons(_, _))

  def concat[A](l: List[List[A]]): List[A] = 
    l match
      case Cons(h, t) => append(h, concat(t))
      case Nil => Nil

  def incrementEach(l: List[Int]): List[Int] =
    foldRight(l, Nil: List[Int], (h, l) => Cons(h+1, l))

  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String], (h, l) => Cons(h.toString, l))

  def map[A,B](l: List[A], f: A => B): List[B] =
    foldRight(l, Nil: List[B], (h, l) => Cons(f(h), l))

  def filter[A](as: List[A], f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A], (h, l) => if f(h) then Cons(h, l) else l)

  def flatMap[A,B](as: List[A], f: A => List[B]): List[B] =
    concat(map(as, f))

  def filterViaFlatMap[A](as: List[A], f: A => Boolean): List[A] =
    flatMap(as, (h) => if f(h) then Cons(h, Nil) else Nil)

  def addPairwise(a: List[Int], b: List[Int]): List[Int] =
    (a, b) match
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1+h2, addPairwise(t1, t2))

  // def zipWith - TODO determine signature

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
    def startsWith[A](as: List[A], bs: List[A]): Boolean = 
      (as, bs) match
        case (_, Nil) => true
        case (Nil, Cons(h, t)) => false
        case (Cons(ha, ta), Cons(hb, tb)) => if ha != hb then false else startsWith(ta, tb)
    sup match
      case Nil => sub == Nil
      case _ if startsWith(sup, sub) => true
      case Cons(h,t) => hasSubsequence(t, sub)
