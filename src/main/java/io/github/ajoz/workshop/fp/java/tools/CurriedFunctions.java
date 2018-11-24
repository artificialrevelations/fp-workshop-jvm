package io.github.ajoz.workshop.fp.java.tools;

public final class CurriedFunctions {

    static <A, B, C> Function1<B, Function1<A, C>> flip(final Function1<A, Function1<B, C>> function1) {
        return (B b) -> (A a) -> function1.apply(a).apply(b);
    }
}
