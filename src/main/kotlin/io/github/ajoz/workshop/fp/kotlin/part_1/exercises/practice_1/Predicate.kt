@file:Suppress("PackageName", "UNUSED_PARAMETER", "unused")

package io.github.ajoz.workshop.fp.kotlin.part_1.exercises.practice_1

/*
  -- Practice: Other type of functions --

  Kotlin does not distinguish between primitives and non primitives, thus a
  special "Predicate" type is not needed like in Java. A predicate can be easily
  expressed with just a simple (A) -> Boolean function.

  This does not mean that we can't make working with such functions easier.
 */

/*
  Part 1:

  Create an extension function called `and` on a (A) -> Boolean type. It should
  take another predicate as an argument and return a predicate that is the logical
  "and" of the results of those two predicates.
 */
infix fun <A> ((A) -> Boolean).and(other: (A) -> Boolean): (A) -> Boolean =
        TODO("Practice Predicate.and is missing!")

/*
  Part 2:

  Create an extension function called `or` on a (A) -> Boolean type. It should
  take another predicate as an argument and return a predicate that is the logical
  "or" of the results of those two predicates.
 */
infix fun <A> ((A) -> Boolean).or(other: (A) -> Boolean): (A) -> Boolean =
        TODO("Practice Predicate.or is missing!")

/*
  Part 3:

  Create a function `not` that returns a Predicate that is a negation.
 */
fun <A> ((A) -> Boolean).not(): (A) -> Boolean =
        TODO("Practice Predicate.not is missing!")

/*
  Part 4:

  Create a function `xor` (exclusive or), try to implement it only with the
  use of `and`, `or`, `not`.
 */
infix fun <A> ((A) -> Boolean).xor(other: (A) -> Boolean): (A) -> Boolean =
        TODO("Practice Predicate.xor is missing!")


/*
  Part 5:

  Please create a predicate called `isAllowed` that returns true if the given
  Int is between 0 to 6 or is equal to 42. Please use predicates defined below
  to solve this exercise.
 */
val isLargerThen0: (Int) -> Boolean = { it > 0 }
val isLowerThen6: (Int) -> Boolean = { it < 6 }
val isEqualTo42: (Int) -> Boolean = { it == 42 }

val isAllowed: (Int) -> Boolean =
        TODO("Practice isAllowed is missing!")