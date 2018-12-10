package io.github.ajoz.workshop.fp.kotlin.tools

import java.util.NoSuchElementException

object EmptyIterator : Iterator<Nothing> {
    override fun hasNext() = false
    override fun next() =
            throw NoSuchElementException("Empty iterator has no values!")
}
