@file:Suppress("UNUSED_PARAMETER", "unused")

package io.github.ajoz.workshop.fp.part1.practice2

import kotlin.reflect.KClass

/*
  -- Fun with Predicates vol. 1 --

  In the previous exercise we've added a lot of useful extension methods for the
  predicate (A) -> Boolean. They allow building more complex predicates, the
  only missing part is library of a general purpose predicates that can be used
  as building blocks.
 */
/*
  Part 1:

  Please create a function called `instanceOf` that takes a Class<A> as an
  argument and returns a predicate (B) -> Boolean. This predicate should be able
  to check if the value is of the given type.

  Hints:
  - Use KClass.isInstance method

  Note:
  - the generic type A and B are both marked as derived from Any, because
    by default Kotlin's generics have upper bound implicitly set to Any?
    Using normal generic that extends Any? would cause a compilation error
    when passing it to isAssignableFrom e.g. it::class.java
 */
fun <A, B: Any> instanceOf(type: KClass<B>): (A) -> Boolean =
        TODO("Practice 2 instanceOf is missing!")

// BONUS: this could be implemented with reified generics

inline fun <A, reified B> instanceOf(): (A) -> Boolean = {
    TODO("Practice 2 reified instanceOf is missing!")
}

/*
  Part 2:

  Please create a function `alwaysTrue` that returns a predicate (A) -> Boolean
  that for any given value will return true.

  Hints:
  - There is not catch in this exercise!
 */
fun <A> alwaysTrue(): (A) -> Boolean =
        TODO("Practice 2 alwaysTrue is missing!")

/*
  Part 3:

  Please create a function `alwaysFalse` that returns a predicate (A) -> Boolean
  that for any given value will return false.

  Hints:
  - There is not catch in this exercise!
 */
fun <A> alwaysFalse(): (A) -> Boolean =
        TODO("Practice 2 alwaysFalse is missing!")

/*
  Part 4:

  Please create a function called `isEqualTo` that returns a predicate that
  checks if its argument is equal to the given value.
*/
fun <A> isEqualTo(value: A): (A) -> Boolean =
        TODO("Practice 2 isEqualTo is missing!")

/*
  Part 5:

  Please create a function called `isLargerThen` that returns a predicate
  that checks if its argument is larger then the given value.
 */
fun isLargerThen(value: Int): (Int) -> Boolean =
        TODO("Practice 2 isLargerThen is missing!")

/*
  Part 6:

  Please create a function called `isLowerThen` that returns a predicate
  that checks if its argument is lower then the given value.
 */
fun isLowerThen(value: Int): (Int) -> Boolean =
        TODO("Practice 2 isLowerThen is missing!")

/*
  Part 7:

  Please create a predicate (Int) -> Boolean called `isAllowed` that returns
  true if the given Integer is between 0 and 6 or equal to 42. Please use the
  predicates defined below to solve this.
 */
val isAllowed: (Int) -> Boolean = {
    TODO("Practice 2 isAllowed is missing!")
}