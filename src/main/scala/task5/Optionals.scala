package task5


object Optionals:
  /**
   * Optional is a type that represents a value that may or may not be present.
   * Similar to Optional in Java but using the ADT concept.
   * Therefore, an Optional is a sum type with two cases: Maybe and Empty.
   * Maybe contains the value, and Empty represents the absence of a value.
   *
   * @tparam A
   */
  enum Optional[A]:
    case Maybe(value: A)
    case Empty()

  object Optional:
    /**
     * isEmpty returns true if the optional is Empty, false otherwise.
     * Example:
     *
     * isEmpty(Empty()) == true
     * isEmpty(Maybe(1)) == false
     *
     * @param optional the optional to check
     * @tparam A the type of the optional
     * @return true if the optional is Empty, false otherwise
     */
    def isEmpty[A](optional: Optional[A]): Boolean = optional match
      case Empty() => true
      case _ => false

    /**
     *
     * getOrElse returns the value of the optional if it is Maybe, otherwise it returns the default value.
     * Example:
     * orElse(Maybe(1), 0) == 1
     * orElse(Empty(), 0) == 0
     *
     * @param optional the optional to get the value from
     * @param default the default value to return if the optional is Empty
     * @tparam A the type of the optional
     * @tparam B the type of the default value
     * @return the value of the optional if it is Maybe, otherwise the default value
     */
    def orElse[A, B >: A](optional: Optional[A], default: B): B = optional match
      case Maybe(value) => value
      case Empty() => default

    /**
     * map applies the function f to the value of the optional if it is Maybe, otherwise it returns Empty.
     * Example:
     *
     * map(Maybe(1), (x: Int) => x + 1) == Maybe(2)
     * map(Empty(), (x: Int) => x + 1) == Empty()
     *
     *
     * @param optional the optional to apply the function to
     * @param f the function to apply to the value of the optional
     * @tparam A the type of the optional
     * @tparam B the type of the result of the function
     * @return the result of applying the function to the value of the optional if it is Maybe, otherwise Empty
     */
    def map[A, B](optional: Optional[A], f: A => B): Optional[B] =
      (optional, f) match
        case (optional, _) => optional match
          case Maybe(v) => Optional.Maybe(f(v))
          case Empty() => Empty()

    /**
     * filter keeps the value if it satisfies the given predicate p and return Maybe value, otherwise it returns Empty
     * Example:
     *
     * filter(Maybe(5))(_ > 2) == Maybe(5)
     * filter(Maybe(5))(_ > 8) == Empty()
     * filter(Empty())(_ > 2) == Empty()
     *
     * @param optional the optional to apply the function to
     * @param p        the predicate to filter the value of the optional
     * @tparam A the type of the optional
     * @return Maybe of value if predicate is satisfied and not Empty, otherwise Empty
     */
    def filter[A](optional: Optional[A])(p: A => Boolean): Optional[A] =
      (optional, p) match
        case (optional, _) => optional match
          case Maybe(v: A) if p(v) => optional
          case _ => Empty()