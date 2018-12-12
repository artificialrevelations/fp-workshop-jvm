package io.github.ajoz.workshop.fp.java.tools.seq;

import io.github.ajoz.workshop.fp.java.tools.Try;

import java.util.Iterator;

@SuppressWarnings("unused")
public final class IteratorFlow<A> implements Flow<A> {
    private final Iterator<A> iterator;

    public IteratorFlow(final Iterator<A> iterator) {
        this.iterator = iterator;
    }

    @Override
    public Try<A> next() {
        // We can manually check if there are any elements and then return
        // Failure or Success accordingly

        // if (!iterator.hasNext())
        //     return Try.failure(new NoSuchElementException("Iterator does not have more elements!"));
        //
        // return Try.success(iterator.next());

        // usage of Try.ofSupplier might be more concise as calling next will always
        // result with an exception if there are no more elements, the main
        // problem lies in the efficiency because the exception needs to be thrown
        return Try.of(iterator::next);
    }
}
