@file:Suppress("unused")

package io.github.ajoz.workshop.fp.kotlin.tools.flow

import io.github.ajoz.workshop.fp.kotlin.tools.control.Try
import java.util.NoSuchElementException

/**
 * Creates a [Flow] from a given array of values.
 *
 * Each element of the given array will be used by the Flow. In case of the
 * array being empty a [Try.Failure] will be send to the downstream. In case of
 * sending all the available elements downstream this Flow will only return
 * [Try.Failure].
 *
 * @param A the type of the returned flow.
 * @param[values] array of values that should be lazily returned in the form of
 * a flow.
 * @return instance of a Flow.
 */
fun <A> Flow.Companion.of(vararg values: A): Flow<A> {
    class ArrayFlow(val array: Array<out A>) : Flow<A> {
        private var current: Int = 0

        override fun next(): Try<A> {
            if (array.isEmpty())
                return Try.Failure(NoSuchElementException("No elements in this Flow!"))

            if (current >= array.size)
                return Try.Failure(NoSuchElementException("No more elements in this Flow!"))

            val next: Try<A> = Try.Success(array[current])
            current++
            return next
        }
    }

    return ArrayFlow(values)
}


