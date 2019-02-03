package io.github.ajoz.workshop.fp.java.part_1.solutions.practice_1;

import io.github.ajoz.workshop.fp.java.tools.Function1;

@FunctionalInterface
public interface Predicate<A> {
    boolean test(final A value);

    // Part 2:
    default Predicate<A> and(final Predicate<A> other) {
        return value -> this.test(value) && other.test(value);
    }

    // Part3:
    default Predicate<A> or(final Predicate<A> other) {
        return value -> this.test(value) || other.test(value);
    }

    // Part 4:
    default Predicate<A> not() {
        return value -> !this.test(value);
    }

    // Part 5:
    default Predicate<A> xor(final Predicate<A> other) {
        // could be: return (this.or(other)).and((this.and(other).not()));
        // Less object creation the way below.
        return value -> {
            final boolean p = this.test(value);
            final boolean q = other.test(value);
            return (p || q) && !(p && q);
        };
    }

    // Part 6:
    default Function1<A, Boolean> asFunction1() {
        return this::test;
    }
}
