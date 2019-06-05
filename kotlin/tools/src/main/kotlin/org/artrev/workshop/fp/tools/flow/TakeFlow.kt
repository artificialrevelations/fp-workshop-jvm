package org.artrev.workshop.fp.tools.flow

import org.artrev.workshop.fp.tools.control.Try
import java.util.NoSuchElementException

fun <A> Flow<A>.take(amount: Int): Flow<A> {
    class TakeFlow(val upstream: Flow<A>,
                   val threshold: Int) : Flow<A> {
        private var taken: Int = 0

        override fun next(): Try<A> {
            // take an element from the upstream Flow
            val next = upstream.next()
            // if the element does not exist then just propagate the failure
            if (next.isFailure)
                return next

            // if already taken enough elements then just propagate the failure
            if (taken >= threshold) {
                return Try.Failure(NoSuchElementException("Reached the Flow take threshold: $threshold"))
            }

            // increment the currently taken amount
            taken++
            // return the upstream Flow item
            return next
        }
    }

    return TakeFlow(this, amount)
}
