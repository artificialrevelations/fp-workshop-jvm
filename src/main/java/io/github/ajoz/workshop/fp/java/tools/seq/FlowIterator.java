package io.github.ajoz.workshop.fp.java.tools.seq;

import io.github.ajoz.workshop.fp.java.tools.Try;

import java.util.Iterator;
import java.util.NoSuchElementException;

public final class FlowIterator<A> implements Iterator<A> {
    private final Flow<A> flow;

    private boolean shouldCheck = true;
    private Try<A> next = Try.failure(new NoSuchElementException("No more elements in this Flow!"));

    FlowIterator(final Flow<A> flow) {
        this.flow = flow;
    }

    @Override
    public boolean hasNext() {
        if (shouldCheck) {
            next = flow.next();
            shouldCheck = false;
        }

        return next.isSuccess();
    }

    @Override
    public A next() {
        final A value = next.get();
        shouldCheck = true;
        return value;
    }
}
