package org.artrev.workshop.fp.tools.flow

import org.artrev.workshop.fp.tools.control.Try

/**
 *
 */
fun <A, B> Flow<A>.map(function: (A) -> B): Flow<B> {
    class MapFlow<A, B>(val upstream: Flow<A>,
                        val mapper: (A) -> B) : Flow<B> {

        override fun next(): Try<B> =
                upstream.next().map(mapper)
    }

    return MapFlow(this, function)
}
