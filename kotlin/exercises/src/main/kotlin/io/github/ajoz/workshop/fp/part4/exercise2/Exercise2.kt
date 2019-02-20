@file:Suppress("PackageName", "unused")

package io.github.ajoz.workshop.fp.part4.exercise2

import java.io.File
import java.io.IOException

/*
  -- Memoization --

  In the previous exercise we modified the `AccidentMetadata` class to use the
  supplier `() -> A` and be fully immutable. Although we made the accident
  loading lazy we made it worse then the original, because the file is read on
  each call to `data` property.

  Was it worth to change the code to a supplier form?

  In the beginning of the workshop we tried to define what FP is.

  One of the core characteristics was composition. The power of FP as a paradigm
  lies in the possibility of building bigger "things" from smaller "things".

  Although it's possible to do it in standard "OO" or procedural languages, FP
  gives us tools to make it easier.

  We moved to the supplier implementation to help us express a bigger problem
  with a smaller "thing". Another advantage of this is the standardized "form".
  It's easier to build if we know the shape of our bricks? Lego bricks? :-)

  Back to the problem at hand. We do not want to call `loadAccidents` multiple
  times.

  The FP answer to this problem is memoization. The term was created in 1968.
  Derived from a latin word "memorandum". It is an "optimization" technique used
  to speed up the execution by storing the results of time-consuming functions,
  a memoized function returns the cached result if the same input occurs.

  Memoization is a trade-off, we exchange execution time for the memory consumption.
  As with all "optimization" techniques it should be used with careful
  consideration.
 */

/*
  Part 1:

  Please create two additional methods for the supplier `() -> A`:
  - default method called `memoized` that returns a supplier that will store the
    value, once it is generated it will always return the "cached" result in
    subsequent calls

  Hints:
  - do not worry about the using the supplier between several threads
  - we need to store the value somehow (maybe mutation?)

  Questions:
  - what kind of functions pure or impure can be memoized?
  - why?
 */

internal fun <A> (() -> A).memoized(): () -> A =
        TODO("Exercise 2 Supplier.memoized is missing!")

/*
  Part 2:

  Please modify the `AccidentMetadata` class implementation to use the newly
  created memoization capabilities of the supplier.

  Hints:
  - are there a lot of changes required?
 */
internal class AccidentMetadata(val id: Long, val source: File) {
    val data: () -> String = ::loadAccident

    private fun loadAccident(): String {
        try {
            return source.readLines().reduce { a, b -> a + b }
        } catch (e: IOException) {
            throw SourceFileCorrupted("Error when reading: ${source.absolutePath}")
        }
    }

    class SourceFileCorrupted internal constructor(message: String) : RuntimeException(message)
}

fun main(args: Array<String>) {
    val file = File("src/main/resources/part4/accident.info")
    val metadata = AccidentMetadata(42L, file)

    // Let's see the console and check if the accident data is correctly
    // memoized
    var data = "error!"
    for (i in 0..99) {
        data = metadata.data()
    }

    println("accident data = $data")
}
