package org.artrev.workshop.fp.tools.flow;

import org.artrev.workshop.fp.tools.Function1;
import org.artrev.workshop.fp.tools.control.Try;

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
