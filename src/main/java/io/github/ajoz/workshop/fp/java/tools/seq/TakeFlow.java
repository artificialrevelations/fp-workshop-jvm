package io.github.ajoz.workshop.fp.java.tools.seq;

import io.github.ajoz.workshop.fp.java.tools.Try;

import java.util.NoSuchElementException;

public final class TakeFlow<A> implements Flow<A> {
    private final Flow<A> upstream;
    private final int amount;

    private int taken;

    TakeFlow(final Flow<A> upstream,
             final int amount) {
        this.upstream = upstream;
        this.amount = amount;
    }

    @Override
    public Try<A> next() {
        // take an element from the upstream Flow
        final Try<A> next = upstream.next();
        // if the elemenent does not exist then just propagate the failure
        if (next.isFailure())
            return next;

        // if already taken enough elements then just propagate the failure
        if (taken >= amount) {
            return Try.failure(new NoSuchElementException("Reached the Flow amount: " + amount));
        }

        // increment the currently taken amount
        taken++;
        // return the upstream Flow item
        return next;
    }
}
