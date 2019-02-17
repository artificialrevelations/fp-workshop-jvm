package io.github.ajoz.workshop.fp.java.part4.solutions.practice_3;

import io.github.ajoz.workshop.fp.java.tools.Consumer1;
import io.github.ajoz.workshop.fp.java.tools.Function1;
import io.github.ajoz.workshop.fp.java.tools.Predicate;
import io.github.ajoz.workshop.fp.java.tools.control.Try;

import java.util.*;

@SuppressWarnings({"unused", "UnusedReturnValue"})
interface Flow<A> {
    Try<A> next();

    default <B> Flow<B> flatMap(final Function1<A, Flow<B>> mapper) {
        return new FlatMapFlow<>(this, mapper);
    }

    default Flow<A> filter(final Predicate<A> predicate) {
        return new FilterFlow<>(this, predicate);
    }

    default Flow<A> peek(final Consumer1<A> action) {
        return new PeekFlow<>(this, action);
    }

    default Flow<A> distinct() {
        return new DistinctFlow<>(this);
    }

    default List<A> toList() {
        final List<A> list = new LinkedList<>();
        while (true) {
            final Try<A> element = next();
            if (element.isFailure())
                break;

            list.add(element.get());
        }
        return list;
    }

    static <A> Flow<A> of(final List<A> list) {
        return new ListFlow<>(list);
    }
}

// Part 1
final class FlatMapFlow<A, B> implements Flow<B> {
    private final Flow<A> upstream;
    private final Function1<A, Flow<B>> mapper;
    private Flow<B> next;

    FlatMapFlow(final Flow<A> upstream,
                final Function1<A, Flow<B>> mapper) {
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
            next = nextUpstreamElement.map(mapper).get();
        } while (true);
    }
}

// Part 2:
final class FilterFlow<A> implements Flow<A> {
    private final Flow<A> upstream;
    private final Predicate<? super A> predicate;

    FilterFlow(final Flow<A> upstream,
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

// Part 3:
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

// Part 4:
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

final class ListFlow<A> implements Flow<A> {
    private final List<A> list;
    private int current;

    ListFlow(final List<A> list) {
        this.list = list;
    }

    @Override
    public Try<A> next() {
        if (list.size() == 0)
            return Try.failure(new NoSuchElementException("No elements in this Flow!"));

        if (current >= list.size())
            return Try.failure(new NoSuchElementException("No more elements in this Flow!"));

        final Try<A> next = Try.success(list.get(current));
        current++;
        return next;
    }
}

public class Practice3 {
    public static void main(String[] args) {
        final List<String> strings =
                Flow.of(Arrays.asList(1, 2, 3))
                        .peek(element -> System.out.println("#before filter -> " + element))
                        .filter(element -> element >= 2)
                        .peek(element -> System.out.println("#before flatMap -> " + element))
                        .flatMap(element -> Flow.of(Arrays.asList(">" + element + "<", ">" + element + "<")))
                        .peek(element -> System.out.println("#before distinct -> " + element))
                        .distinct()
                        .toList();
        System.out.println("strings = " + strings);
    }
}
