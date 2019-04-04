package org.artrev.workshop.fp.tools.iterators

import java.util.NoSuchElementException

object EmptyIterator : Iterator<Nothing> {
    override fun hasNext() = false
    override fun next() =
            throw NoSuchElementException("Empty iterator has no values!")
}
