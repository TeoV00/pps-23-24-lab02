
package task2a

import org.junit.Assert.*
import org.junit.Test
import task2a.Lab02.Shapes.Shape.{Circle, Rectangle, Square}
import task2a.Lab02.Shapes.*
import task5.Optionals.Optional
import task5.Optionals.Optional.*

import scala.annotation.tailrec

object Lab02 extends App:

  /**
   * Task 2
   */
  //3a
  val positive: Int => String =
    _ match
      case n if n >= 0 => "positive"
      case _ => "negative"

  def positiveMethod(n: Int): String =
    n match
      case n if n >= 0 => "positive"
      case _ => "negative"

  class PositiveTest:
    @Test def testPositive() : Unit =
      assertEquals("positive", positive(3))
      assertEquals("negative", positive(-2))

    @Test def testPositiveMethod(): Unit =
      assertEquals("positive", positiveMethod(3))
      assertEquals("negative", positiveMethod(-2))

  //3b
  val neg: (String => Boolean) => (String => Boolean) =
    f => (s => !f(s))

  def negM(p: String => Boolean): (String => Boolean) =
    s => !p(s)

  //3c
  def neg[X](p: X => Boolean): (X => Boolean) =
    (s: X) => !p(s)

  class NegTest:
    val predStringEmpty: String => Boolean = s => s == ""

    @Test def testNeg(): Unit =
      assertTrue(negM(predStringEmpty)("ciao"))
      assertTrue(neg(predStringEmpty)("ciao"))

    @Test def testGenericNeg(): Unit =
      assertTrue(neg[String](predStringEmpty)("pluto"))
      assertFalse(neg[Int](i => i == 3)(3))

  //4
  val p1: Int => Int => Int => Boolean = x => y => z => x <= y && y == z
  val p2: (Int, Int, Int) => Boolean = (x, y, z) => (y == z) && x <= y
  def p3(x: Int)(y: Int)(z: Int): Boolean = (y == z) && x <= y
  def p4(x: Int, y: Int, z: Int): Boolean = (y == z) && x <= y

  class CurryingTest:
    @Test def testCurriedFunType(): Unit =
      assertTrue(p1(3)(4)(4))
      assertFalse(p1(5)(4)(4))

    @Test def testNonCurriedFunType(): Unit =
      assertTrue(p2(3, 4, 4))
      assertFalse(p2(3, 4, 5))

    @Test def testCurriedMethod(): Unit =
      assertTrue(p3(3)(4)(4))
      assertFalse(p3(5)(4)(1))

    @Test def testNonCurriedMethod(): Unit =
      assertTrue(p4(3, 4, 4))
      assertFalse(p4(3, 1, 5))

  //5
  def compose(f: Int => Int, g: Int => Int): Int => Int = i => f(g(i))
  def composeG[A, B, C](f: B => C, g: A => B): A => C = (i: A) => f(g(i))

  class ComposeTest:
    @Test def testCompose(): Unit =
      assertEquals(19, compose(_ + 3, _ * 2)(8))

    @Test def testGenericCompose(): Unit =
      assertEquals("16.0", composeG[Int, Double, String](v => s"$v", _ * 2.0)(8))

  /**
   * Task 3
   */
  @tailrec
  def gcd(a:Int, b: Int): Int =
    (a, b) match
      case (a, 0) => a
      case _ if a > b => gcd(b, (a % b))

  class Task3Test:
    @Test def testGcd(): Unit =
      assertEquals(4, gcd(12, 8))
      assertEquals(7, gcd(14, 7))
      assertEquals(5, gcd(20, 15))

  /**
   * Task 4
   */
  object Shapes:
    enum Shape:
      case Rectangle(b: Double, h: Double)
      case Square(e: Double)
      case Circle(r: Double)

    @tailrec
    def perimeter(s: Shape): Double =
      s match
        case Rectangle(b, h) => (b + h) * 2
        case Square(e) => perimeter(Rectangle(e, e))
        case Circle(r) => 2 * Math.PI * r

    def scale(s: Shape, a: Double): Shape =
      (s, a) match
        case (Rectangle(b, h), a) => Rectangle(b * a, h * a)
        case (Square(e), a) => Square(e * a)
        case (Circle(r), a) => Circle(r * a)

  class ShapeTest:
    val noToll = 0.0
    val scaleAplha = 2.0

    @Test def testPerimeterRectangle(): Unit =
      val rectPerim = 12.0;
      assertEquals(rectPerim, perimeter(Rectangle(4, 2)), noToll)

    @Test def testScaleRectangle(): Unit =
      val scaledRect = Rectangle(4, 6)
      assertEquals(scaledRect, scale(Rectangle(2, 3), scaleAplha))

    @Test def testPerimeterSquare(): Unit =
      val squarePerim = 8.0
      assertEquals(squarePerim, perimeter(Square(2.0)), noToll)

    @Test def testScaleSquare(): Unit =
      val scaledSquare = Square(4)
      assertEquals(scaledSquare, scale(Square(2), scaleAplha))

    @Test def testPerimeterCircle(): Unit =
      val circlePerim = 25.13
      val toll = 0.2
      assertEquals(circlePerim, perimeter(Circle(4)), toll)

    @Test def testScaleCircle(): Unit =
      val scaledCircle = Circle(6)
      assertEquals(scaledCircle, scale(Circle(3), scaleAplha))

  /**
   * Task 5
   */
  def map[A, B](optional: Optional[A], f: A => B): Optional[B] =
    (optional, f) match
      case (optional, _) => optional match
        case Maybe(v) => Optional.Maybe(f(v))
        case Empty() => Empty()

  def filter[A](optional: Optional[A])(f: A => Boolean): Optional[A] =
    (optional, f) match
      case (optional, _) => optional match
        case Maybe(v: A) if f(v) => optional
        case _ => Empty()
  @Test def filterShouldReturnEmptyWhenPredicateNotSatisfied(): Unit = {
    val nonEmpty = Optional.Maybe(5)
    val empty = Optional.Empty[Int]()
    assertEquals(nonEmpty, filter(nonEmpty)(_ > 2))
    assertEquals(Optional.Empty(), filter(nonEmpty)(_ > 8))
    assertEquals(Optional.Empty(), filter(empty)(_ > 2))
  }