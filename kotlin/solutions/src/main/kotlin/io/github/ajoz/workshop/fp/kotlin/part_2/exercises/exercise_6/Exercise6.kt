@file:Suppress("PackageName")

package io.github.ajoz.workshop.fp.kotlin.part_2.exercises.exercise_6

/*
   Part 1:

   Create a function called `foldLeftCurried` that perform a foldLeft but
   in curried form.
  */
fun <A, B> foldLeftCurried(operator: (B) -> (A) -> B): (B) -> (List<A>) -> B {
    throw UnsupportedOperationException("Exercise 6 foldLeftCurried is missing!")
}

/*
   Part 2:

   Create a function called `sum` that is using `foldLeftCurried`
   Create a function called `product` that is using `foldLeftCurried`
  */

/*
    var sum =

    var product =
  */

/*
  Part 3:

  Create a function called `mapCurried` that perform a map but in curried form.
 */
fun <A, B> mapCurried(mapper: (A) -> B): (List<A>) -> List<B> {
    throw UnsupportedOperationException("Exercise 6 mapCurried is missing!")
}

/*
   Part 4:

   Create a function called `lengths` that is using `mapCurried`.
  */

/*
   var lengths =
 */
