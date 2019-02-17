package io.github.ajoz.workshop.fp.java.tools.flow;

import io.github.ajoz.workshop.fp.java.tools.*;
import io.github.ajoz.workshop.fp.java.tools.control.Try;

import java.util.Iterator;
import java.util.List;

/*
 This is an example of a Iterator<A> without the need for specialized hasNext
 method. Classic Java Iterator<A> defines three methods:

 - boolean hasNext()
 - A next()
 - void remove()

 The remove method can be treated as an additional operation, that is not
 associated in any way with the main concern of an Iterator which is just
 iteration.

 Although hasNext() and next() seem simple, they are unsafe. Calling next()
 without checking hasNext() might result with an exception depending on the
 implementation. This can be avoided if a type that can express uncertenity
 is used: Option, Optional, Maybe, Try or even Either.

 For this simple implementation Try instead of Option as it allows sending
 information about the cause of a failed computation.

 Why the name Flow? So it's not confused with an Iterator. But the name is
 similar enough so it rings a bell.

 Also didn't choose Seq, Sequence or Stream for similar reason.
*/
@SuppressWarnings("unused")
public interface Flow<A> extends Iterable<A> {

    Try<A> next();

    default <B> Flow<B> map(final Function1<? super A, ? extends B> mapper) {
        return new MapFlow<>(this, mapper);
    }

    default <B> Flow<B> flatMap(final Function1<? super A, ? extends Flow<? extends B>> mapper) {
        return new FlatMapFlow<>(this, mapper);
    }

    default Flow<A> filter(final Predicate<? super A> predicate) {
        return new FilterFlow<>(this, predicate);
    }

    default Flow<A> peek(final Consumer1<? super A> action) {
        return new PeekFlow<>(this, action);
    }

    default Flow<A> distinct() {
        return new DistinctFlow<>(this);
    }

    // there is no type that would allow expressing a need for positive integers
    // without zero :> like natural numbers
    default Flow<A> take(final int amount) {
        return new TakeFlow<>(this, amount);
    }

    // this is a terminal operation
    default List<A> toList() {
        return Flows.toList(this);
    }

    default <B> B foldLeft(final B initial, Function2<B, A, B> function) {
        return Flows.foldLeft(function, initial, this);
    }

    default Iterator<A> iterator() {
        return new FlowIterator<>(this);
    }

    @SafeVarargs
    @SuppressWarnings("varargs") // if creating a Stream from an array is safe then creating an Flow is ;-)
    static <A> Flow<A> from(final A... items) {
        return new ArrayFlow<>(items);
    }

    static <A> Flow<A> cycle(final A... items) {
        return new CycleArrayFlow<>(items);
    }

    static <A> Flow<A> generate(final A seed, final Function1<A, A> generator) {
        return new Seed1Flow<>(seed, generator);
    }

    static <A, B, C> Flow<C> zip(final Flow<A> left,
                                 final Flow<B> right,
                                 final Function2<A, B, C> zipper) {
        return new ZippingFlow<>(left, right, zipper);
    }

    static <A> Flow<A> empty() {
        return new EmptyFlow<>();
    }
}
