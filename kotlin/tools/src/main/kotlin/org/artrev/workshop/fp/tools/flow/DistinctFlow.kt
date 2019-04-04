package org.artrev.workshop.fp.tools.flow

import io.github.ajoz.workshop.fp.tools.control.Try
import java.util.HashSet
import java.util.NoSuchElementException

/**
 * Returns a [Flow] that only passes downstream distinct elements.
 *
 * @param A the type of the returned flow.
 * @return instance of a Flow.
 */
fun <A> Flow<A>.distinct(): Flow<A> {
    class DistinctFlow(val upstream: Flow<A>) : Flow<A> {
        val distinctElements: MutableSet<A> = HashSet()

        override fun next(): Try<A> {
            do {
                val next = upstream.next()
                if (next.isFailure) {
                    distinctElements.clear()
                    return Try.Failure(NoSuchElementException("No more elements in distinct Flow!"))
                }

                val a = next.get()
                if (distinctElements.add(a)) {
                    return Try.Success(a)
                }
            } while (true)
        }
    }

    return DistinctFlow(this)
}

