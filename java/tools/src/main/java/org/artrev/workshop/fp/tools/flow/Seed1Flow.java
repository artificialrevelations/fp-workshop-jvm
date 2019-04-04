package org.artrev.workshop.fp.tools.flow;

import org.artrev.workshop.fp.tools.Function1;
import org.artrev.workshop.fp.tools.control.Try;

public final class Seed1Flow<A> implements Flow<A> {
    private final Function1<A, A> generator;
    private A seed;

    public Seed1Flow(final A seed,
                     final Function1<A, A> generator) {
        this.seed = seed;
        this.generator = generator;
    }

    @Override
    public Try<A> next() {
        final Try<A> next = Try.success(seed);
        // should this halt if an exception occurs in generator function?
        seed = generator.apply(seed);
        return next;
    }
}
