package io.github.ajoz.workshop.fp.part_1.solutions.practice;

@FunctionalInterface
interface Predicate<A> {
    boolean test(final A value);

    // foo
    default Predicate<A> and(final Predicate<A> other) {
        return value -> this.test(value) && other.test(value);
    }

    // bar
    default Predicate<A> or(final Predicate<A> other) {
        return value -> this.test(value) || other.test(value);
    }
}
