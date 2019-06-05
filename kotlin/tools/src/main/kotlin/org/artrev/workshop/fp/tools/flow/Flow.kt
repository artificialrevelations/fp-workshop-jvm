package org.artrev.workshop.fp.tools.flow

import org.artrev.workshop.fp.tools.control.Try
import java.util.NoSuchElementException

interface Flow<A> : Iterable<A> {
    fun next(): Try<A>

    override fun iterator(): Iterator<A> = object : Iterator<A> {
        private var shouldCheck = true
        private var next: Try<A> =
                Try.Failure(NoSuchElementException("No more elements in this Flow!"))

        override fun hasNext(): Boolean {
            if (shouldCheck) {
                next = this@Flow.next()
                shouldCheck = false
            }

            return next.isSuccess
        }

        override fun next(): A {
            hasNext()
            val value = next.get()
            shouldCheck = true
            return value
        }
    }

    companion object
}