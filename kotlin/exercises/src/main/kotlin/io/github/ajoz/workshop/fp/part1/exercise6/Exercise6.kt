@file:Suppress("UNUSED_PARAMETER", "UnusedMainParameter", "unused", "UNUSED_VARIABLE")

package io.github.ajoz.workshop.fp.part1.exercise6

import io.github.ajoz.workshop.fp.tools.andThen

/*
  -- Other types of Functions --

  In the Java 8 standard library you can find few additional "function-like"
  types:
  - Supplier<T> which is just a Function<Void, T>
  - Consumer<T> which is just a Function<T, Void>

  Why the fuss? Simply due to the peculiar nature of the Void type. This type
  before Java 5 (when generics were introduced) was used mainly in conjunction
  with reflection.

  In Kotlin the type system is more "modern" and we can cleanly express the
  "supplier" and "consumer" with usual KFunction1 and Unit type or can't we?
*/

object IssuesWithTheUnit {
    @JvmStatic
    fun main(args: Array<String>) {
        val supplyInt: (Unit) -> Int = { 42 }
        val consumeInt: (Int) -> Unit = { println(it) }

        val blackHole = supplyInt.andThen(consumeInt)

        blackHole(Unit) //a wee bit better then passing a null

        /*
          (Unit) -> Unit is not the same as () -> Unit simply because the first
          one is compiled to Function1 and the second one to Function0, beware!
          Check the tools.jvm.functions to look at the types
         */

        val supplyInt2: () -> Int = { 42 }

        val blackHole2 = supplyInt2.andThen(consumeInt)
        blackHole2() // calling it is easier but it is not a real A -> B function :(
    }
}

/*
  Are such "functions" useful? We talked about pure and impure functions, we also
  talked about referential transparency. So are pure, referential transparent
  functions like this useful?

  1) A pure referential transparent function from Unit to T would need to return
  the same result when called each time and it would not be very interesting.

  A Random.nextInt() seems like a nice candidate, but it would have to return the
  same int each time.

  2) A pure referential transparent function from T to Unit is even less
  interesting as this function would eat up any input and return the same result.

  A print(T) seems like a nice candidate, but it should just eat up the argument
  and do nothing.

  That's the theory but in practice suppliers and consumers are used to model
  some impure effects like reading from a network resource or writing to a
  database. Such modeling of impure effects can be done better ofc.
 */

/*
  Part 1:

  Please create an extension function called `compose` on a consumer (B) -> Unit
  that takes a one argument function (A) -> B and returns a consumer (A) -> Unit
 */
infix fun <A, B> ((B) -> Unit).compose(function: (A) -> B): (A) -> Unit =
        TODO("Exercise 6 Consumer.compose is missing!")

/*
  Part 2:

  Please use the newly created extension function `compose` to compose
  - a consumer that prints Integers
  - a function that returns length of a String
 */
internal object ComposingConsumer {
    @JvmStatic
    fun main(args: Array<String>) {
        val printInt: (Int) -> Unit = { println(it) }
        val strlen: (String) -> Int = { it.length }

        val printStrLen: (String) -> Unit = {
            TODO("Exercise 6 printStrLen is missing!")
        }

        printStrLen("https://www.meetup.com/Java-User-Group-Lodz/")
    }
}

/*
  Part 3:

  Please create an extension function called `andThen` on a supplier () -> A
  that takes a one argument function (A) -> B and returns a supplier () -> B
 */
infix fun <A, B> (() -> A).andThen(function: (A) -> B): () -> B =
        TODO("Exercise 6 Supplier.andThen is missing!")

/*
  Part 4:

  Please use the newly created extension function `andThen` to compose
  - a supplier that returns a String
  - a function that returns length of a String
 */
internal object ComposingSupplier {
    @JvmStatic
    fun main(args: Array<String>) {
        val getFacebook = { "https://www.facebook.com/groups/juglodz/" }
        val strlen: (String) -> Int = { it.length }

        val getFBLen: () -> Int = {
            TODO("Exercise 6 getFBLen is missing!")
        }

        println(getFBLen())
    }
}

/*
  Part 5:

  Please create an extension function called `applyFirst` to supply the first
  argument of a curried two argument function.
 */
fun <A, B, C> ((A) -> (B) -> C).applyFirst(supplier: () -> A): (B) -> C =
        TODO("Exercise 6 applyFirst is missing!")

/*
  Part 6:

  Please create an extension function called `applyFirst` to supply the second
  argument of a curried two argument function.
 */
fun <A, B, C> ((A) -> (B) -> C).applySecond(supplier: () -> B): (A) -> C =
        TODO("Exercise 6 applySecond is missing!")