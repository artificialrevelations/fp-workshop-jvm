@file:Suppress("PackageName", "unused")

package io.github.ajoz.workshop.fp.kotlin.part_4.exercises.exercise_3

/*
  -- Everything Memoized! --

  In the previous exercise we managed to memoize the `Supplier`, can we do the
  same with the `Function1` and `Function2` types?

  The interface `Supplier<A>` is just a `Function1<Void, A>` in disguise. Thus
  it should be easy to change all the `() -> A` into `A -> B`?
 */

/*
  Part 1:

  Please create two additional methods for the Function1:
  - default method called `memoized` that returns a one argument function that
    will store it's result value and return it each time on subsequent calls.
  - static method called `memoize` that takes a one argument function as an
    argument and returns its memoized version.

  Hints:
  - do not worry about the using the function between several threads
  - we need to store the value somehow (maybe mutation?)

  Questions:
  - what kind of functions pure or impure can be memoized?
  - why?
 */

internal fun <A, B> ((A) -> B).memoized(): (A) -> B =
        TODO("Exercise 3 Function1.memoized is missing!")

/*
  Part 2:

  Please create two additional methods for the Function2:
  - default method called `memoized` that returns a two argument function that
    will store it's result value and return it each time on subsequent calls.
  - static method called `memoize` that takes a two argument function as an
    argument and returns its memoized version.

  Hints:
  - do not worry about the using the function between several threads
  - we need to store the value somehow (maybe mutation?)

  Questions:
  - what kind of functions pure or impure can be memoized?
  - why?
 */

internal fun <A, B, C> ((A, B) -> C).memoized(): (A, B) -> C =
        TODO("Exercise 3 Function2.memoized is missing!")

fun main(args: Array<String>) {
    // Part 1:
    val fun1: (String) -> Int = { arg: String ->
        println(String.format("Argument passed: %s", arg))
        arg.length
    }.memoized()

    (0..9).forEach { println(fun1("#$it JUG Lodz")) }
    (0..9).forEach { println(fun1("#$it Mobilization Conference")) }

    // Part 2:
    val fun2: (String, String) -> Int = { arg1: String, arg2: String ->
        println(String.format("Arguments passed: %s, %s", arg1, arg2))
        (arg1 + arg2).length
    }.memoized()

    (0..9).forEach { println(fun2("#$it JUG", "#$it Lodz")) }
    (0..9).forEach { println(fun2("#$it Mobilization", "#$it Conference")) }
}
