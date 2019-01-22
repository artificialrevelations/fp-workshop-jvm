package io.github.ajoz.workshop.fp.java.tools.flow;

import io.github.ajoz.workshop.fp.java.tools.Consumer1;
import io.github.ajoz.workshop.fp.java.tools.control.Try;

final class PeekFlow<A> implements Flow<A> {
    private final Flow<A> upstream;
    private final Consumer1<? super A> action;

    PeekFlow(final Flow<A> upstream,
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
