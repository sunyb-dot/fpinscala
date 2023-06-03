package fpinscala.exercises.errorhandling

// Hide std library `Option` since we are writing our own in this chapter
import scala.{Option as _, Some as _, None as _}

enum Option[+A]:
  case Some(get: A)
  case None

  def map[B](f: A => B): Option[B] =
    this match
      case Some(a) => Some(f(a))
      case None => None

  def getOrElse[B>:A](default: => B): B =
    this match
      case Some(a) => a
      case _ => default

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f).getOrElse(None)

  def orElse[B>:A](ob: => Option[B]): Option[B] =
    map(Some(_)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] =
    flatMap(a => if f(a) then Some(a) else None)

object Option:

  def failingFn(i: Int): Int =
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try
      val x = 42 + 5
      x + y
    catch case e: Exception => 43 // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.

  def failingFn2(i: Int): Int =
    try
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    catch case e: Exception => 43

  def mean(xs: Seq[Double]): Option[Double] =
    if xs.isEmpty then None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) match
      case None => None
      case Some(m) => mean(xs.map(_ - m).map(a => a*a))

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    (a, b) match
      case (Some(a), Some(b)) => Some(f(a, b))
      case _ => None

  def sequence[A](as: List[Option[A]]): Option[List[A]] =
    as match
      case Nil => Some(Nil)
      case head :: next => head match
        case None => None
        case Some(h) => sequence(next) match
          case None => None
          case Some(n) => Some(h :: n)

  def sequence2[A](as: List[Option[A]]): Option[List[A]] =
    as match
      case Nil => Some(Nil)
      case head :: next => head.flatMap(hh => sequence2(next).map(hh :: _))

  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    sequence(as.map(f))
