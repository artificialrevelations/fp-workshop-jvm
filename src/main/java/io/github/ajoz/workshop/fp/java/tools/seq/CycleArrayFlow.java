package io.github.ajoz.workshop.fp.java.tools.seq;

import io.github.ajoz.workshop.fp.java.tools.Try;

import java.util.NoSuchElementException;

public final class CycleArrayFlow<A> implements Flow<A> {
    private final A[] array;
    private int current;

    @SafeVarargs
    CycleArrayFlow(final A... array) {
        this.array = array;
    }

    @Override
    public Try<A> next() {
        if (array.length == 0)
            return Try.failure(new NoSuchElementException("No elements in this Flow!"));

        if (current >= array.length)
            current = 0; //we move the cycle to the beginning

        final Try<A> next = Try.success(array[current]);
        current++;
        return next;
    }
}
