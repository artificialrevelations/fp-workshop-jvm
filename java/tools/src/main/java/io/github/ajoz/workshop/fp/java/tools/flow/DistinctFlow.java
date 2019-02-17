package io.github.ajoz.workshop.fp.java.tools.flow;

import io.github.ajoz.workshop.fp.java.tools.control.Try;

import java.util.HashSet;
import java.util.NoSuchElementException;
import java.util.Set;

final class DistinctFlow<A> implements Flow<A> {
    private final Flow<A> mUpstream;
    private final Set<A> mDistinct;

    DistinctFlow(final Flow<A> upstream) {
        mUpstream = upstream;
        mDistinct = new HashSet<>();
    }

    @Override
    public Try<A> next() {
        do {
            final Try<A> next = mUpstream.next();
            if (next.isFailure()) {
                // no more elements in the upstream, we can clear the memory
                mDistinct.clear();
                return Try.failure(new NoSuchElementException("No more elements in distinct Flow!"));
            }

            // if there is an element upstream then check if we can put it in hash set
            // this is a memory hog, it could use some witty optimization but this
            // is basically the same how kotlin is doing this for Sequence
            final A a = next.get();
            if (mDistinct.add(a)) {
                return Try.success(a);
            }
        } while (true);
    }
}