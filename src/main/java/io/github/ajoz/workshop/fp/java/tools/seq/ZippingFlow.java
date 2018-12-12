package io.github.ajoz.workshop.fp.java.tools.seq;

import io.github.ajoz.workshop.fp.java.tools.Function2;
import io.github.ajoz.workshop.fp.java.tools.Try;

import java.util.NoSuchElementException;

class ZippingFlow<A, B, C> implements Flow<C> {
    private final Flow<A> left;
    private final Flow<B> right;
    private final Function2<A, B, C> zipper;

    ZippingFlow(final Flow<A> left,
                final Flow<B> right,
                final Function2<A, B, C> zipper) {
        this.left = left;
        this.right = right;
        this.zipper = zipper;
    }

    @Override
    public Try<C> next() {
        final Try<A> nextLeft = left.next();
        if (nextLeft.isFailure())
            return Try.failure(new NoSuchElementException("Left seq is out of elements to zip!"));

        final Try<B> nextRight = right.next();
        if (nextRight.isFailure())
            return Try.failure(new NoSuchElementException("Right seq is out of elements to zip!"));

        return nextLeft.flatMap(a -> nextRight.flatMap(b -> Try.success(zipper.apply(a, b))));
    }
}
