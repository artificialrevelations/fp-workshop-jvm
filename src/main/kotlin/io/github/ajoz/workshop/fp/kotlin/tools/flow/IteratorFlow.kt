package io.github.ajoz.workshop.fp.kotlin.tools.flow

import io.github.ajoz.workshop.fp.kotlin.tools.Try

fun <A> Flow.Companion.of(iterator: Iterator<A>): Flow<A> {
    class IteratorFlow(val iter: Iterator<A>) : Flow<A> {
        override fun next(): Try<A> {
            // We can manually check if there are any elements and then return
            // Failure or Success accordingly

            // if (!iterator.hasNext())
            //     return Try.failure(new NoSuchElementException("Iterator does not have more elements!"));
            //
            // return Try.success(iterator.next());

            // usage of Try.ofSupplier might be more concise as calling next will always
            // result with an exception if there are no more elements, the main
            // problem lies in the efficiency because the exception needs to be thrown
            return Try.of { iter.next() }
        }
    }

    return IteratorFlow(iterator)
}

