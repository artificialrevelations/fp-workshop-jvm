@file:Suppress("PackageName", "UNUSED_PARAMETER", "unused")

package io.github.ajoz.workshop.fp.part4.exercise4

import java.io.File
import java.io.IOException
import java.util.concurrent.atomic.AtomicReference

/*
  -- Even more laziness --

  Currently the `Supplier` although lazy and memoized is not super useful. We
  are still lacking tools to work with it. The ultimate goal is to create a
  type which can be easily composed.

  Let's start with our old friends: `map` and `flatMap` so far we created them
  for a `List<A>`, a `Maybe<A>`, a `Try<A>` and `Either<A, B>`. If we would
  squint our eyes a little single element `List<A>` doesn't differ much from
  `Maybe<A>`, so does a `Supplier<A>`.

  We need to remember that `Supplier<A>` is a `() -> A`. So if we would like to
  "map" it with a function `A -> B` we need to transform it to a `() -> B`.
 */

/*
  Part 1:

  Please add `map` method to the supplier interface.
 */
internal fun <A, B> (() -> A).map(function: (A) -> B): () -> B =
        TODO("Exercise 4 (() -> A).map is missing!")

/*
  Part 2:

  Please add `flatMap` method to the supplier interface. It is simpler then
  you think.

  Hints:
  - what do we have to do to not have a Supplier<Supplier<B>>?
  - almost like `map` + this one flatten
 */
internal fun <A, B> (() -> A).flatMap(function: (A) -> () -> B): () -> B =
        TODO("Exercise 4 (() -> A).flatMap is missing!")

/*
  Part 3:

  Please add `before` method to the supplier interface. It should take an
  effect as an argument and perform it before returning the value.
 */
internal fun <A> (() -> A).before(effect: () -> Unit): () -> A =
        TODO("Exercise 4 (() -> A).before is missing!")

/*
  Part 4:

  Please add `after` method to the supplier interface. It should take an
  effect as an argument and perform it before returning the value.
 */
internal fun <A> (() -> A).after(effect: (A) -> Unit): () -> A =
        TODO("Exercise 4 (() -> A).after is missing!")

internal fun <A> (() -> A).memoized(): () -> A {
    val memo: AtomicReference<A> = AtomicReference()
    return {
        synchronized(memo) {
            if (memo.get() == null) memo.set(invoke())
            memo.get()
        }
    }
}

/*
  Part 5:

  After talking with other teams working on the Accident user stories, you and
  your team decide to rework the `AccidentMetadata` API so it's much safer for
  the user.

  Instead of returning a bare String as data, you painstakingly designed a better
  type called `Accident`. It will keep all the parsed information. Users of your
  glorious `AccidentMetadata` class won't ever know how the underlying information
  is structured.

  Some day maybe, just maybe you will move to a more human readable majestic
  format like XML instead of this plain and stupid CSV.

  Please modify the `AccidentMetadata` so it's possible to:
  - print to logs the ID of the accident before the loadAccident is performed
  - split the line from the file over the commas
  - transform the resulting string array into Accident object
  - print to logs the result
  - remember that the result should be memoized

 */
internal data class Accident(
        val latitude: Double,
        val longitude: Double,
        val message: String
)

internal class AccidentMetadata(val id: Long, val source: File) {
    val data: () -> String = ::loadAccident.memoized()

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
    // Part 1:
    val s1 = { "JUG Lodz" }
    val s1map = s1.map(String::length)
    println("s1map  = " + s1map())

    // Part 2:
    val s2 = { "Checkout our meetup" }
    val s2flatMap = s2.flatMap { string -> { string.length } }
    println("s2flatMap  = " + s2flatMap())

    // Part 3:
    val s3 = { "and facebook page" }
    s3.before { println("Happens before!") }
    println("s2flatMap  = " + s3())

    // Part 4:
    val s4 = { "We are organizing Mobilization conference!" }
    s4.after { string -> println("Happens after: $string") }
    println("s2flatMap  = " + s4())


}
