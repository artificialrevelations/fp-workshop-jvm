package io.github.ajoz.workshop.fp.java.tools.seq;

import io.github.ajoz.workshop.fp.java.tools.Function1;
import io.github.ajoz.workshop.fp.java.tools.Try;

public final class MapFlow<A, B> implements Flow<B> {
    private final Flow<A> upstream;
    private final Function1<? super A, ? extends B> mapper;

    public MapFlow(final Flow<A> upstream,
                   final Function1<? super A, ? extends B> mapper) {
        this.upstream = upstream;
        this.mapper = mapper;
    }

    @Override
    public Try<B> next() {
        return upstream.next().map(mapper);
    }
}
