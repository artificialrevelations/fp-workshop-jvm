package io.github.ajoz.workshop.fp.java.part_1.solutions.practice_1;

import io.github.ajoz.workshop.fp.java.tools.Function1;

@FunctionalInterface
public interface Predicate<A> {
    boolean test(final A value);

    // Part 1:
    default Predicate<A> and(final Predicate<A> other) {
        return value -> this.test(value) && other.test(value);
    }

    // Part 2:
    default Predicate<A> or(final Predicate<A> other) {
        return value -> this.test(value) || other.test(value);
    }

    // Part 3:
    default Predicate<A> not() {
        return value -> !this.test(value);
    }

    // Part 4:
    default Predicate<A> xor(final Predicate<A> other) {
        // could be: return (this.or(other)).and((this.and(other).not()));
        // Less object creation the way below.
        return value -> {
            final boolean p = this.test(value);
            final boolean q = other.test(value);
            return (p || q) && !(p && q);
        };
    }

    // Part 5:
    default Function1<A, Boolean> asFunction1() {
        return this::test;
    }
}

@SuppressWarnings({"Convert2MethodRef", "unused"})
class UsingPredicates {
    private static final Predicate<String> isNull = str -> str == null;
    private static final Predicate<String> isEmpty = str -> str.isEmpty();
    private static final Predicate<String> isBlank = str -> str.trim().isEmpty();

    // Part 6:
    static final Predicate<String> isNullOrEmpty = isNull.or(isEmpty);

    // Part 7:
    static final Predicate<String> isNullOrBlank = isNull.or(isBlank);

    // Part 8:
    private static final Predicate<Integer> isLargerThen0 = i -> i > 0;
    private static final Predicate<Integer> isLowerThen6 = i -> i < 6;
    private static final Predicate<Integer> isEqualTo42 = i -> i == 42;

    static final Predicate<Integer> isAllowed =
            isLargerThen0.and(isLowerThen6).or(isEqualTo42);
}
