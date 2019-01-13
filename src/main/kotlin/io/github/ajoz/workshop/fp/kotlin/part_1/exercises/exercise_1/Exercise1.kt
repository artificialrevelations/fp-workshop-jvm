@file:Suppress("PackageName")

package io.github.ajoz.workshop.fp.kotlin.part_1.exercises.exercise_1

/*
  -- Simple Functions --

  In Kotlin we have a few ways to express a function
  - as an abstract class
  - as an interface
  - using a built in KFunction type and a convenient syntax

  A type that can represent a function from Int to Int can look like:

  abstract class IntFunction {
    abstract fun invoke(argument: Int): Int
  }

  This is super verbose way (like in Java), we can shorten things up using
  an interface:

  interface IntFunction {
    fun invoke(argument: Int): Int
  }

  But this is only marginally less verbose then the class version. Kotlin gives
  us a very nice syntax for functions:

  - if we would like to pass an Int to Int function as an argument:

  fun foo(f: (Int) -> Int) {
    // some code
  }

  - if we would like to return an Int to Int function as a result:

  fun bar(): (Int) -> Int {
    // some code
  }

  - if we would like to create a property that holds an Int to Int function:

  val baz: (Int) -> Int = { arg -> // some code }

  The compiler does all the magic and translates this nice notation into
  instances of KFunction1.

  In the Java version of the same exercise we were working towards building
  a specialized type for expressing a function. Due to how Kotlin is designed
  we can focus only on the built in type as it allows for adding extensions.

  Express the function y = x + 1 with your class.
  Express the function y = (x + 2)^2 with your class.
  Express the function y = -x + 100 with you class.
 */

/*
  Part 1:

  Create functions:
  - y = x + 1
  - y = (x + 2)^2
  - y = -x + 10
  - y = x^2 + 4x + 1
  */

// y = x + 1
val f1: (Int) -> Int = { x: Int -> x + 1 }

// y = (x + 2)^2
val f2: (Int) -> Int = TODO("Exercise 1 f2 function is missing!")

// y = -x + 10
val f3: (Int) -> Int = TODO("Exercise 1 f3 function is missing!")

// y = x^2 + 4x + 1
val f4: (Int) -> Int = TODO("Exercise 1 f4 function is missing!")

/*
  Part 2:

  Create a function from String to Int that returns the size of a given String.
 */

val strlen: (String) -> Int = TODO("Exercise 1 strlen is missing!")
