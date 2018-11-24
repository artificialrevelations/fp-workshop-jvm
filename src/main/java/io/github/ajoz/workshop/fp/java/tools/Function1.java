package io.github.ajoz.workshop.fp.java.tools;

@FunctionalInterface
public interface Function1<A, B> {
    B apply(A a);

    // first applies `this` function and then applies `after` function
    default <C> Function1<A, C> andThen(final Function1<B, C> after) {
        return a -> after.apply(this.apply(a));
    }

    // first applies `before` function and then applies `this` function
    default <C> Function1<C, B> compose(final Function1<C, A> before) {
        return c -> this.apply(before.apply(c));
    }

    static <A> Function1<A, A> identity() {
        return a -> a;
    }

    static <A, B> Function1<A, B> constant(final B b) {
        return ignored -> b;
    }
}