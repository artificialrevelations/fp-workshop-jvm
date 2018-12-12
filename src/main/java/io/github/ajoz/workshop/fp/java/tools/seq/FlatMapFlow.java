package io.github.ajoz.workshop.fp.java.tools.seq;

import io.github.ajoz.workshop.fp.java.tools.Function1;
import io.github.ajoz.workshop.fp.java.tools.Try;

public final class FlatMapFlow<T, R> implements Flow<R> {
    private final Flow<T> upstream;
    private final Function1<? super T, ? extends Flow<? extends R>> mapper;

    public FlatMapFlow(final Flow<T> upstream,
                       final Function1<? super T, ? extends Flow<? extends R>> mapper) {
        this.upstream = upstream;
        this.mapper = mapper;
    }

    @Override
    public Try<R> next() {
        return upstream
                .next()
                .flatMap(t -> mapper.apply(t).next());
    }
}
