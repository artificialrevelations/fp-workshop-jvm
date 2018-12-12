package io.github.ajoz.workshop.fp.java.tools.seq;

import io.github.ajoz.workshop.fp.java.tools.Try;

import java.util.NoSuchElementException;

public final class EmptyFlow<A> implements Flow<A> {
    @Override
    public Try<A> next() {
        return Try.failure(new NoSuchElementException("Empty sequence does not have a next element!"));
    }
}
