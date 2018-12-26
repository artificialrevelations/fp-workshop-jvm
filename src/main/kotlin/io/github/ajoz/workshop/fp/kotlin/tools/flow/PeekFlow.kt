@file:Suppress("unused")

package io.github.ajoz.workshop.fp.kotlin.tools.flow

import io.github.ajoz.workshop.fp.kotlin.tools.Try

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
