package io.github.ajoz.workshop.fp.tools.predicates;

import io.github.ajoz.workshop.fp.tools.Predicate;

public class Predicates {
    static <A, B> Predicate<B> instanceOf(final Class<A> type) {
        return (B b) -> type.isAssignableFrom(b.getClass());
    }

    static <A> Predicate<A> isNull() {
        return (A a) -> a == null;
    }

    static <A> Predicate<A> isNotNull() {
        return Predicates.<A>isNull().not();
    }

    static <A> Predicate<A> alwaysTrue() {
        return a -> true;
    }

    static <A> Predicate<A> alwaysFalse() {
        return a -> false;
    }

    static <A> Predicate<A> isEqualTo(final A value) {
        return (A a) -> value.equals(a);
    }
}