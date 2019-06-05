@file:Suppress("unused")

package org.artrev.workshop.fp.tools.flow

import org.artrev.workshop.fp.tools.control.Try
import java.util.NoSuchElementException

/**
 * Creates an infinite [Flow] from a given array of values.
 *
 * The created flow will send all the elements available in the given array and
 * once reaching the end of the array it will start sending elements from the
 * beginning. In case of the given array being empty a [Try.Failure] will be
 * send to the downstream.
 *
 * @param A the type of the returned flow.
 * @param[values] array of values that should be lazily returned in the form of
 * a flow.
 * @return instance of a Flow.
 */
fun <A> Flow.Companion.cycle(vararg values: A): Flow<A> {
    class CycleArrayFlow(val array: Array<out A>) : Flow<A> {
        private var current: Int = 0

        override fun next(): Try<A> {
            if (array.isEmpty())
                return Try.Failure(NoSuchElementException("No elements in this Flow!"))

            if (current >= array.size)
                current = 0 //we move the cycle to the beginning

            val next: Try<A> = Try.Success(array[current])
            current++
            return next
        }
    }
    return CycleArrayFlow(values)
}
