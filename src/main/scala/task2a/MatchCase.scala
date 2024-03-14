
package task2a

object MatchCase extends App:

  val positive: Int => String = n => n match
    case n if n >= 0 => "positive"
    case _ => "negative"

  println(positive(3))
  println(positive(-3))

  def positiveMethod(n: Int): String = n match
    case n if n >= 0 => "positive"
    case _ => "negative"

  println(positiveMethod(-3))
  println(positiveMethod(3))