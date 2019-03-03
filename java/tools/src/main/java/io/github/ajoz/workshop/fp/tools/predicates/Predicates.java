package io.github.ajoz.workshop.fp.tools.predicates;

import io.github.ajoz.workshop.fp.tools.Predicate;

public class Predicates {
    public static <A, B> Predicate<B> instanceOf(final Class<A> type) {
        return (B b) -> type.isAssignableFrom(b.getClass());
    }

    public static <A> Predicate<A> isNull() {
        return (A a) -> a == null;
    }

    public static <A> Predicate<A> isNotNull() {
        return Predicates.<A>isNull().not();
    }

    public static <A> Predicate<A> alwaysTrue() {
        return a -> true;
    }

    public static <A> Predicate<A> alwaysFalse() {
        return a -> false;
    }

    public static <A> Predicate<A> isEqualTo(final A value) {
        return (A a) -> value.equals(a);
    }
}