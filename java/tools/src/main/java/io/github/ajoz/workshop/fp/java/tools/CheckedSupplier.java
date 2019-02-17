package io.github.ajoz.workshop.fp.java.tools;

public interface CheckedSupplier<A> {
    A get() throws Exception;

    default <B> CheckedSupplier<B> compose(final Function1<A, B> function) {
        return () -> function.apply(this.get());
    }
}
