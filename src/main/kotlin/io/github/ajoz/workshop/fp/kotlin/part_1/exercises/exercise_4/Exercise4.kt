@file:Suppress("PackageName", "UNUSED_PARAMETER", "unused", "UNUSED_VARIABLE")

package io.github.ajoz.workshop.fp.kotlin.part_1.exercises.exercise_4

/*
  -- Two argument Functions --

  Part 1:

  Take a look at the function declarations below:
*/

// this looks very nice
val oneArg: (Int) -> Int = { it }

// this looks almost as nice as the one above
val twoArg: (Int, Int) -> Int = { a, b -> a + b }

// this is getting worse
val threeArg: (Int, Int, Int) -> Int = { a, _, _ -> a }

// this is not workable
val nineArg: (Int, Int, Int, Int, Int, Int, Int, Int, Int) -> Int = { a, _, _, _, _, _, _, _, _ ->
    a
}

/*
  If you are moving to Kotlin from Java and your or your team is in a transition
  period, you could make this Kotlin notation more familiar for people with
  type aliases
 */

// Here type aliases that will be super familiar for folks using Java 8
typealias Function<A, B> = (A) -> B

typealias BiFunction<A, B, C> = (A, B) -> C
// etc...

/*
  The nine argument function like above can be shortened with a typealias, but
  depending on the types of arguments it might not help at all.
 */

/*
  Questions:
  - Does a two argument function or a three argument function (or nine argument
   function for that matter) even exists?
  - Do you see any problems with working with them?
  - Are there ways to make the code look more readable?

  Let's assume they do not exist and try to express them with only one argument
  function. We can wrap every argument into a single type.

  So Function2 is really a Function1 that has a >> pair << passed as an argument.
  In a pseudo code you could write it as:

  (A, B) -> C == (Pair<A, B>) -> C

  The same thing happens for Function3:

  (A, B, C) -> D == (Triple<A, B, C>) -> D

  We need special types like Pair or Triple. Some languages have support for
  tuples but in java we need to make them on our own.
 */

/*
  Part 2:

  Create function called `tuple` that takes a (A, B) -> C as an argument and
  returns a (Pair<A, B>) -> C as a result
 */
fun <A, B, C> tuple(function: (A, B) -> C): (Pair<A, B>) -> C {
    TODO("Exercise 4 tuple is missing!")
}

/*
  Part 3:

  Create function called `unTuple` that takes a (Pair<A, B>) -> C that has a
  Pair as the argument and returns a (A, B) -> C as a result.
 */
fun <A, B, C> unTuple(function: (Pair<A, B>) -> C): (A, B) -> C {
    TODO("Exercise 4 unTuple is missing!")
}

/*
      Part 4:

      Please read the functions below and use the newly created functions `tuple`
      and `untuple` to fix the example code.

      The suffixes added are:
      - UT - untupled
      - T - tupled
     */
internal object TupleExample {
    val addPrefixUT: (String, String) -> String = { prefix, text ->
        prefix + text
    }

    val addIntsT: (Pair<Int, Int>) -> Int = { pint ->
        pint.first + pint.second
    }

    @JvmStatic
    fun main(args: Array<String>) {
        val addPrefixT: (Pair<String, String>) -> String = {
            TODO("Exercise 4 addPrefixT is missing!")
        }

        println(addPrefixT(Pair("https://", "nozama.com")))

        val addIntsUT: (Int, Int) -> Int = { _, _ ->
            TODO("Exercise 4 addIntsUT is missing!")
        }

        println(addIntsUT(42, 0))
    }
}

/*
  Let's try to define a two argument functions in terms of a one argument function
  again, this time without aiding ourselves with another class. Let's look at the
  two argument function declaration again:

  (A, B) -> C

  If we can only pass a single argument, let's say the A what do we have:

  (A) -> ???

  If we are going to pass only A, we are missing B to compute the C, this
  means that we are missing part of the computation, this means that we could
  split the computation. We could split it in as many parts as there are
  arguments.

  (A, B) -> C == (A) -> (B) -> C

  Our three argument function would look like:

  (A, B, C) -> D == (A) -> (B) -> (C) -> D

  This method of expressing functions is called currying.
  */
// Example:
// Most verbose version
val strPlusInt1: (String) -> (Int) -> String = { string: String ->
    { int: Int ->
        "$string$int"
    }
}

// More concise
val strPlusInt2: (String) -> (Int) -> String = { string ->
    { int ->
        "$string$int"
    }
}

/*
  Part 5:

  Create function called `curry` that takes a (A, B) -> C as an argument and
  returns its curried representation.
*/
fun <A, B, C> curry(function: (A, B) -> C): (A) -> (B) -> C {
    TODO("Exercise 4 curry is missing!")
}

/*
  Part 6:

  Create function called `uncurry` that takes a curried two argument function
  and returns its (A, B) -> C representation.
 */
fun <A, B, C> unCurry(function: (A) -> (B) -> C): (A, B) -> C {
    TODO("Exercise 4 unCurry is missing!")
}

/*
      Part 7:

      Please read the functions below and use the newly created functions `curry`
      and `uncurry` to fix the example code.

      The suffixes added are:
      - UC - uncurried
      - C - curried
     */
internal object CurryExample {
    @JvmStatic
    fun main(args: Array<String>) {
        // curried
        // simple int multiplication
        val multiplyC: (Int) -> (Int) -> Int = {
            TODO("Exercise 4 multiplyC is missing!")
        }

        // use `uncurry` function to solve this
        val multiplyUC: (Int, Int) -> Int = { _, _ ->
            TODO("Exercise 4 multiplyUC is missing!")
        }

        println(multiplyUC(42, 1))

        // uncurried:
        // repeat the same text specified number of times
        val replicateUC: (Int, String) -> String = { times, str ->
            val result = StringBuilder()
            for (i in 0 until times) {
                result.append(str)
            }
            result.toString()
        }

        val replicateC: (Int) -> (String) -> String = {
            TODO("Exercise 4 replicateC is missing")
        }

        println(replicateC(42)("JUG"))
    }
}

/*
  Part 8:

  Useful functions. Sometimes we are working with an API that does accept
  functions of certain signature. This can be problematic if our own API
  has slightly different signature i.e. order or arguments is different.

  Create function `flip` that takes a two argument function as an argument and
  returns another two argument function but with the arguments flipped.

  Example:
  (A, B) -> C after flip becomes a (B, A) -> C
 */
fun <A, B, C> flip(function: (A, B) -> C): (B, A) -> C {
    TODO("Exercise 4 flip is missing!")
}

/*
  Part 9:

  Create function `flipTupled` that takes a (Pair<A, B>) -> C with a pair argument
  and returns another (Pair<B, A>) -> C with a pair argument, this time the order of
  arguments in the pair should be switched.
 */
fun <A, B, C> flipTupled(function: (Pair<A, B>) -> C): (Pair<B, A>) -> C {
    TODO("Exercise 4 flipTupled is missing!")
}

/*
  Part 10:

  Create function `flipCurried` that takes a curried function and returns a
  curried function with the argument switched. A function a -> b -> c after
  flipCurried should be b -> a -> c
 */
fun <A, B, C> flipCurried(function: (A) -> (B) -> C): (B) -> (A) -> C {
    TODO("Exercise 4 flipCurried is missing!")
}


