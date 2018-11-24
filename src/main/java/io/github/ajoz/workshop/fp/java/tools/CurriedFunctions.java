package io.github.ajoz.workshop.fp.java.tools;

public final class CurriedFunctions {

    public static <A, B, C> Function1<B, Function1<A, C>> flip(final Function1<A, Function1<B, C>> function1) {
        return (B b) -> (A a) -> function1.apply(a).apply(b);
    }

    public static <A, B, C> Function1<B, C> applyFirst(final Function1<A, Function1<B, C>> function,
                                                       final Supplier<A> supplier) {
        return (B b) -> function.apply(supplier.get()).apply(b);
    }

    public static <A, B, C> Function1<A, C> applySecond(final Function1<A, Function1<B, C>> function,
                                                        final Supplier<B> supplier) {
        return (A a) -> function.apply(a).apply(supplier.get());
    }
}
