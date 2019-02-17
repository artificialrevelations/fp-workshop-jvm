@file:Suppress("PackageName", "UNUSED_PARAMETER", "unused")

package io.github.ajoz.workshop.fp.kotlin.part_3.exercises.exercise_5

/*
  -- Partial and Total Functions

  A partial function from X to Y is a function f: X' -> Y for some proper subset
  X' of X. If X' = X, then f is called a total function.

  This definition sounds very "math-y" but in plain words it means that partial
  functions are those that do not have "results" for every argument in their
  domain.

  Do such functions exist?

  The simplest example is division. We cannot divide by zero. So division is a
  partial function on the domain of natural numbers but is a total function on
  the domain of natural numbers without zero.

  But this is "math-y", what about real world engineering examples?

  Are these functions partial?
  - getting information from the Database
  - getting information from the Cache
  - getting information from the Server
  - getting information from a Device
  - getting value from a Map under the given key
  - getting first value from a List (head)
  - getting the List except the head (tail)
  - getting List item on the given index
  - getting element from a Set
  - getting value stored in SharedPreferences (Android)

  This looks like a lot of things we are usually working with.
 */
/*
  Part 1:

  Please create a function called `div1` that performs division of two
  given Integers.

  Question:
  - what will happen if we divide by zero?
  - is having an implicit exception in the code a good thing?
  - how can we solve this issue?
 */
fun div1(a: Int, b: Int): Int =
        TODO("Exercise 4 div1 is missing!")

/*
  Part 2:

  Please create a function called `div2` that performs division of two
  given Integers but in the case of the second argument being zero throws
  a checked exception called DivideByZero.

  Question:
  - is using a checked exception better?
  - what about the code readability?
  - what about the code extendability?
  - is it easy to use the `div2` function with other functions?
 */
class DivideByZero : Exception()

@Throws(DivideByZero::class)
fun div2(a: Int, b: Int): Int =
        TODO("Exercise 4 div2 is missing!")

/*
  Part 3:

  Please create a function called `div3` that performs division of two
  given Integers but in the case of the second argument being zero it
  returns a null.

  Question:
  - is it better then the checked exception version?
  - what about the code readability?
  - what about the code extendability?
  - is it easier to use then `div2`?
 */
fun div3(a: Int, b: Int): Int? =
        TODO("Exercise 4 div3 is missing!")

/*
   Part 4:

   Please create a function called `div4` that performs division of two
   given Integers, it should return a class Result that holds the actual
   result or information if it exists.

   Question:
   - can this be solved with a Sum type?
   - how should such sum type look like?
 */
class Result(val value: Int, val exists: Boolean)

fun div4(a: Int, b: Int): Result =
        TODO("Exercise 4 div4 is missing!")

/*
  Part 5:

  Please create a sum type called `Maybe` that can either be Some value
  or None, then create a function called `safeDiv` that is using it as
  a result.

  Hints:
  - data Maybe a = Some a | None
  - None can be an object but does it need a specific generic?
 */
sealed class Maybe<A>

fun safeDiv(a: Int, b: Int): Maybe<Int> =
        TODO("Exercise 4 safeDiv is missing!")

fun main(args: Array<String>) {
    // Part 1:
    println(div1(42, 0))

    // Part 2:
    try {
        val res2 = div2(42, 0)
        println(res2)
    } catch (divideByZero: DivideByZero) {
        divideByZero.printStackTrace()
    }

    // Part 3:
    val res3 = div3(42, 0)
    if (null != res3) {
        println("Div3 result: $res3")
    } else {
        println("Error handling after div3 failed!")
    }

    // Part 4:
    val res4 = div4(42, 0)
    if (res4.exists) {
        println("Div4 result: $res4")
    } else {
        println("Error handling after div4 failed!")
    }

    // Part 5:
//    val res5 = safeDiv(42, 0)
//    when (res5) {
//        is Maybe.Some -> println("SafeDiv result: ${res5.value}")
//        is Maybe.None -> println("Error handling after safeDiv failed!")
//    }
}