package io.github.ajoz.workshop.fp.part_1.solutions.exercise_4;

import kotlin.Pair;

@FunctionalInterface
interface Function1<A, B> {
    B apply(A a);
}

@FunctionalInterface
interface Function2<A, B, C> {
    C apply(A a, B b);
}

class Exercise4 {
    static <A, B, C> Function1<Pair<A, B>, C> convertToFunction1WithPair(final Function2<A, B, C> function2) {
        return tuple -> function2.apply(tuple.getFirst(), tuple.getSecond());
    }

    static <A, B, C> Function1<A, Function1<B, C>> convertToFunction1WithFunction(final Function2<A, B, C> function2) {
        return (A a) -> (B b) -> function2.apply(a, b);
    }

    static <A, B, C> Function2<A, B, C> convertToFunction2FromPair(final Function1<Pair<A, B>, C> function1) {
        return (A a, B b) -> function1.apply(new Pair<>(a, b));
    }

    static <A, B, C> Function2<A, B, C> convertToFunction2FromFunction(final Function1<A, Function1<B, C>> function1) {
        return (A a, B b) -> function1.apply(a).apply(b);
    }
}