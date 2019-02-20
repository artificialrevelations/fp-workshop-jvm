@file:Suppress("PackageName", "unused", "UNUSED_PARAMETER")

package io.github.ajoz.workshop.fp.part4.practice1

import io.github.ajoz.workshop.fp.tools.control.Maybe
import io.github.ajoz.workshop.fp.tools.control.Try
import java.util.concurrent.atomic.AtomicReference

/*
  -- Further Enhancing the Supplier --

  We defined a Supplier as:

  interface Supplier<A> {
      A get();
  }

  This is a very nice definition but might be problematic if we would like to
  work with any kind of method that Kotlin has to offer. This means working with
  methods that might throw an exception.
 */

/*
  Part 1:

  Please implement default method `tryGet` that returns an instance of
  `Try<A>` instead of just `A`. It should return Try.Failure if using `get`
  would cause an exception, it should return the result wrapped inside the
  Try.Success otherwise.
 */
internal fun <A> (() -> A).tryGet(): Try<A> =
        TODO("Practice 1 Supplier.tryGet is missing!")

/*
  Part 2:

  Please implement default method `maybeGet` that returns an instance of
  `Maybe<A>` instead of `A`. It should return Maybe.None if using `get`
  would cause an exception, it should return the result wrapped inside the
  Maybe.Some otherwise.
 */
internal fun <A> (() -> A).maybeGet(): Maybe<A> =
        TODO("Practice 1 Supplier.tryGet is missing!")

/*
  Part 3:

  Please implement default method `getOrElse` that returns a supplied value
  in case the operation deferred by the Supplier throws an exception.
 */
internal fun <A> (() -> A).getOrElse(default: A): A =
        TODO("Practice 1 Supplier.getOrElse is missing!")

internal fun <A> (() -> A).memoized(): () -> A {
    val memo: AtomicReference<A> = AtomicReference()
    return {
        synchronized(memo) {
            if (memo.get() == null) memo.set(invoke())
            memo.get()
        }
    }
}

internal class SomethingHappenedToFoo : Exception("Something very very very bad happened!")

/*
  Part 4:

  What will happen if an exception is thrown in a memoized supplier?
 */
@Throws(Exception::class)
fun foo(): String {
    throw SomethingHappenedToFoo()
}

fun main(args: Array<String>) {
    val supplier = ::foo

    try {
        supplier()
    } catch (e: Exception) {
        println(String.format("Exception thrown: %s", e.message))
    }

    supplier.maybeGet()
            .ifSome { println(it) }
            .ifNone { println("none result :(") }

    supplier.tryGet()
            .ifSuccess { println(it) }
            .ifFailure { println(it) }

    println(supplier.getOrElse("Value in case of error!"))
}
