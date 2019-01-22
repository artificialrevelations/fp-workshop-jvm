@file:Suppress("PackageName")

package io.github.ajoz.workshop.fp.kotlin.part_4.solutions.practice_1

import io.github.ajoz.workshop.fp.kotlin.tools.control.Maybe
import io.github.ajoz.workshop.fp.kotlin.tools.control.Try
import java.util.concurrent.atomic.AtomicReference

internal fun <A> (() -> A).tryGet(): Try<A> =
        Try.of(this)

internal fun <A> (() -> A).maybeGet(): Maybe<A> = try {
    Maybe.Some(invoke())
} catch (exception: Exception) {
    Maybe.None()
}

internal fun <A> (() -> A).getOrElse(default: A): A = try {
    invoke()
} catch (exception: Exception) {
    default
}

internal fun <A> (() -> A).memoized(): () -> A {
    val memo: AtomicReference<A> = AtomicReference()
    return {
        synchronized(memo) {
            if (memo.get() == null)
                memo.set(invoke())
            memo.get()
        }
    }
}

internal class SomethingHappenedToFoo : Exception("Something very very very bad happened!")

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

    val memoized = supplier.memoized()
    for (i in 0..9) {
        memoized.maybeGet()
                .ifSome { println(it) }
                .ifNone { println("none result :(") }

        memoized.tryGet()
                .ifSuccess { println(it) }
                .ifFailure { println(it) }
    }
}
