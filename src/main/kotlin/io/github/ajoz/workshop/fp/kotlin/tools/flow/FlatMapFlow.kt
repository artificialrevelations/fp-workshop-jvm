package io.github.ajoz.workshop.fp.kotlin.tools.flow

import io.github.ajoz.workshop.fp.kotlin.tools.Try
import java.util.NoSuchElementException

/**
 *
 */
fun <A, B> Flow<A>.flatMap(function: (A) -> Flow<B>): Flow<B> {
    class FlatMapFlow(val upstream: Flow<A>,
                      val mapper: (A) -> Flow<B>) : Flow<B> {
        private var next: Flow<B>? = null

        override fun next(): Try<B> {
            do {
                val nextDownstreamElement = Try
                        .ofNullable(next)
                        .flatMap { it.next() }

                if (nextDownstreamElement.isSuccess)
                    return nextDownstreamElement

                val nextUpstreamElement = upstream.next()

                if (nextUpstreamElement.isFailure)
                    return Try.Failure(NoSuchElementException("No more elements to flatMap in upstream Flow!"))

                next = nextUpstreamElement.map(mapper).get()
            } while (true)
        }
    }

    return FlatMapFlow(this, function)
}

