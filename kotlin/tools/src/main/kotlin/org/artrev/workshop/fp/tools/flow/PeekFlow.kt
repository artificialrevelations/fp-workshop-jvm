@file:Suppress("unused")

package org.artrev.workshop.fp.tools.flow

import io.github.ajoz.workshop.fp.tools.control.Try

/**
 *
 */
fun <A> Flow<A>.peek(effect: (A) -> Unit): Flow<A> {
    class PeekFlow<A>(val upstream: Flow<A>,
                      val action: (A) -> Unit) : Flow<A> {

        override fun next(): Try<A> =
                upstream.next().ifSuccess(action)
    }

    return PeekFlow(this, effect)
}
