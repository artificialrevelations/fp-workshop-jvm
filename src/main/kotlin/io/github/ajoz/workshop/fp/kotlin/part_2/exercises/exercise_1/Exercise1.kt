@file:Suppress("PackageName", "UNUSED_PARAMETER")

package io.github.ajoz.workshop.fp.kotlin.part_2.exercises.exercise_1

/*
  -- Simple functions --

 */

/*
  Part 1:

  Please create a function called `sum` that takes a List of Integers
  and returns the sum of those integers.
  - please consider a case of an empty list
  - passed list will never be null
  - passed list will never have null elements
 */
fun sum(list: List<Int>): Int {
    TODO("Exercise 1 sum is missing!")
}

/*
  Part 2:

  Please create a function called `product` that takes a List of Integers
  and returns a product of those integers.
  - please consider a case of an empty list
  - passed list will never be null
  - passed list will never have null elements
 */
fun product(list: List<Int>): Int {
    TODO("Exercise 1 product is missing!")
}

/*
  Part 3:

  Please look at the implementation of `sum` and `product`
  - can you show the parts that are different?
  - can you show the parts that are the same?
  - can you abstract the code in a way that it can be used for sum and product?
 */

/*
  // for the abstracted function we will use a name `foo`
  // hint:
  // - what arguments do we need to pass to `foo`?
  // - `sum` is using + operator, `product` is using * operator
  //   how can we abstract these two operators?
  // - do not use any conditional statements for this exercise
  fun foo(??): Int {
      TODO("Exercise 1 foo is missing!")
  }

  fun sum2(list: List<Int>): Int {
      return foo(list, ...)
  }

  fun product2(list: List<Int>): Int {
      return foo(list, ...)
  }
 */