package org.artrev.workshop.fp.tools.flow;

import org.artrev.workshop.fp.tools.Function1;
import org.artrev.workshop.fp.tools.control.Try;

import java.util.NoSuchElementException;

final class FlatMapFlow<A, B> implements Flow<B> {
    private final Flow<A> upstream;
    private final Function1<? super A, ? extends Flow<? extends B>> mapper;
    private Flow<B> next;

    FlatMapFlow(final Flow<A> upstream,
                final Function1<? super A, ? extends Flow<? extends B>> mapper) {
        this.upstream = upstream;
        this.mapper = mapper;
    }

    @Override
    public Try<B> next() {
        do {
            // if we are already have next Flow then take an element of it
            // and pass it as next to downstream
            final Try<B> nextDownstreamElement =
                    Try.ofNullable(next)
                            .flatMap(Flow::next);

            // if this is a Success we can pass it
            if (nextDownstreamElement.isSuccess()) {
                return nextDownstreamElement;
            }

            // if this is a Failure this means that either we:
            // - started flatMapping (next is null)
            // - next finished and returned a Failure because there are no more elements in it
            final Try<A> nextUpstreamElement = upstream.next();

            // if there are no more elements in the Flow then just return a Failure
            if (nextUpstreamElement.isFailure()) {
                return Try.failure(new NoSuchElementException("No more elements to flatMap in upstream Flow!"));
            }

            // if there is an element in the upstream Flow then flatMap it and set as nextFlow
            // we need to then put elements from this retrieved Flow into downstream
            //noinspection unchecked
            next = (Flow<B>) nextUpstreamElement.map(mapper).get();
        } while (true);
    }
}
