package io.github.ajoz.workshop.fp.java.tools.seq;

import io.github.ajoz.workshop.fp.java.tools.Consumer1;
import io.github.ajoz.workshop.fp.java.tools.Try;

public final class OnEachFlow<A> implements Flow<A> {
    private final Flow<A> upstream;
    private final Consumer1<? super A> action;

    public OnEachFlow(final Flow<A> upstream,
                      final Consumer1<? super A> action) {
        this.upstream = upstream;
        this.action = action;
    }

    @Override
    public Try<A> next() {
        // Try implementation has ifSucces already so it's not necessary
        // to implement it manually
        return upstream.next().ifSuccess(action);
    }
}
