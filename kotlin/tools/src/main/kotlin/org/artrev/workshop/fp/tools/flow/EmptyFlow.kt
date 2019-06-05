package org.artrev.workshop.fp.tools.flow

import org.artrev.workshop.fp.tools.control.Try
import java.util.NoSuchElementException

/**
 * Creates an empty [Flow] of elements.
 *
 * Empty flow will immediately return [Try.Failure] when an item is queried.
 *
 * @return instance of Flow.
 */
fun <A> Flow.Companion.empty(): Flow<A> {
    class EmptyFlow : Flow<A> {
        override fun next(): Try<A> =
                Try.Failure(NoSuchElementException("Empty Flow does not have a next element!"))
    }

    return EmptyFlow()
}


