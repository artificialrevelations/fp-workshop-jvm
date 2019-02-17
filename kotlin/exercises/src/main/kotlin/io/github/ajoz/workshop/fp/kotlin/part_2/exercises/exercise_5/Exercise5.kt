@file:Suppress("PackageName", "UNUSED_PARAMETER")

package io.github.ajoz.workshop.fp.kotlin.part_2.exercises.exercise_5

/*
  -- Other useful functions --

  The world of higher order functions does not end on fold. There are many
  other useful function that are begging to be written and used.
 */

/*
  Part 1:

  Create a function called `mapInts` that takes a list of Integers as an
  argument and a one argument function. This `mapInts` should return a List
  of Integers that are result of applying the given function.

  Hints:
  - new list should have the same size and old list
  - for empty list an empty list should be returned
  - mapInts([1, 2, 3], x -> x + 1) == [2, 3, 4]
 */
internal fun mapInts(list: List<Int>,
                     mapper: (Int) -> Int): List<Int> {
    throw UnsupportedOperationException("Exercise 5 mapInts is missing!")
}

/*
  Part 2:

  Create a function called `map` that is a generalization of `mapInts`.
  It should take a List of elements of type A and a one argument function.
  It should be possible to express `mapInts` with this new `map`

  Hints:
  - new list should have the same size and old list
  - for empty list an empty list should be returned
  - let the types guide you
  */
internal fun <A, B> map(list: List<A>,
                        mapper: (A) -> B): List<B> {
    throw UnsupportedOperationException("Exercise 5 map is missing!")
}

/*
  Part 3:

  Create a function called `addOne` that takes a List of Integers and returns
  a List of Integers increased by one. This `addOne` should use the `map`
  function.

  Hints:
  - new list should have the same size and old list
  - for empty list an empty list should be returned
  */
internal fun addOne(list: List<Int>): List<Int> {
    throw UnsupportedOperationException("Exercise 5 addOne is missing!")
}

/*
  Part 4:

  Create a function called `lengths` that takes a List of Strings and returns
  a List of Integers (lengths of those strings). This `lengths` should use
  the `map` function.

  Hints:
  - new list should have the same size and old list
  - for empty list an empty list should be returned
  - you can use String::length
 */
internal fun lengths(strings: List<String>): List<Int> {
    throw UnsupportedOperationException("Exercise 5 lengths is missing!")
}
