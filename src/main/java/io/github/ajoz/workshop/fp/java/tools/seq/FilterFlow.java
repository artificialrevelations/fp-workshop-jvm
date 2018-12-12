package io.github.ajoz.workshop.fp.java.tools.seq;

import io.github.ajoz.workshop.fp.java.tools.Predicate;
import io.github.ajoz.workshop.fp.java.tools.Try;

public final class FilterFlow<A> implements Flow<A> {
    private final Flow<A> upstream;
    private final Predicate<? super A> predicate;

    public FilterFlow(final Flow<A> upstream,
                      final Predicate<? super A> predicate) {
        this.upstream = upstream;
        this.predicate = predicate;
    }

    @Override
    public Try<A> next() {
        do {
            // take the next item from upstream
            final Try<A> next = upstream.next();
            // if there is no more items upstream, then just return a failure
            if (next.isFailure())
                return next;

            // if there is an item upstream, then check if satisfies the
            // given predicate and if it does then return it
            final Try<A> filtered = next.filter(predicate);
            if (filtered.isSuccess())
                return filtered;
            // if the item upstream does not satisfy the predicate then
            // try again
        } while (true);
    }
}
