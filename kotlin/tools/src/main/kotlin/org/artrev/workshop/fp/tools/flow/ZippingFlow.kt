@file:Suppress("unused")

package org.artrev.workshop.fp.tools.flow

import io.github.ajoz.workshop.fp.tools.control.Try
import java.util.NoSuchElementException

/**
 *
 */
fun <A, B, C> Flow<A>.zip(other: Flow<B>, function: (A, B) -> C): Flow<C> {
    class ZippingFlow<A, B, C>(private val left: Flow<A>,
                               private val right: Flow<B>,
                               private val zipper: (A, B) -> C) : Flow<C> {

        override fun next(): Try<C> {
            val nextLeft = left.next()
            if (nextLeft.isFailure)
                return Try.Failure(NoSuchElementException("Left flow is out of elements to zip!"))

            val nextRight = right.next()
            return if (nextRight.isFailure)
                Try.Failure(NoSuchElementException("Right flow is out of elements to zip!"))
            else
                nextLeft.flatMap { a ->
                    nextRight.flatMap { b ->
                        Try.Success(zipper(a, b))
                    }
                }

        }
    }

    return ZippingFlow(this, other, function)
}
