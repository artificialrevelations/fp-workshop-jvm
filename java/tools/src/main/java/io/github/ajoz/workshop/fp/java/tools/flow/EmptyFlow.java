package io.github.ajoz.workshop.fp.java.tools.flow;

import io.github.ajoz.workshop.fp.java.tools.control.Try;

import java.util.NoSuchElementException;

public final class EmptyFlow<A> implements Flow<A> {
    @Override
    public Try<A> next() {
        return Try.failure(new NoSuchElementException("Empty sequence does not have a next element!"));
    }
}
