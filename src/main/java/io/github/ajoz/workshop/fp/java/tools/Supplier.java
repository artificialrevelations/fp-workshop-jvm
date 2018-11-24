package io.github.ajoz.workshop.fp.java.tools;

public interface Supplier<A> {
    A get();

    default <B> Supplier<B> compose(final Function1<A, B> function) {
        return () -> function.apply(this.get());
    }
}
