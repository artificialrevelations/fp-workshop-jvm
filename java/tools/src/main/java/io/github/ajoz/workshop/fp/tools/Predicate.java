package io.github.ajoz.workshop.fp.tools;

@FunctionalInterface
public interface Predicate<A> {
    
    boolean test(final A value);

    // foo
    default Predicate<A> and(final Predicate<A> other) {
        return value -> this.test(value) && other.test(value);
    }

    // bar
    default Predicate<A> or(final Predicate<A> other) {
        return value -> this.test(value) || other.test(value);
    }

    default Predicate<A> not() {
        return value -> !this.test(value);
    }

    default Predicate<A> xor(final Predicate<A> other) {
        return value -> {
            final boolean p = this.test(value);
            final boolean q = other.test(value);
            return (p || q) && !(p && q);
        };
    }

    default Function1<A, Boolean> asFunction1() {
        return this::test;
    }
}
