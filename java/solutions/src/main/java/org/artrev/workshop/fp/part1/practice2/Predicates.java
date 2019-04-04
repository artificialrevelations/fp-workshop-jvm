package org.artrev.workshop.fp.part1.practice2;

import org.artrev.workshop.fp.tools.Predicate;

import static org.artrev.workshop.fp.part1.practice2.IntPredicates.isLargerThen;
import static org.artrev.workshop.fp.part1.practice2.IntPredicates.isLowerThen;
import static org.artrev.workshop.fp.part1.practice2.Predicates.isEqualTo;

@SuppressWarnings({"Convert2MethodRef", "unused", "SameParameterValue"})
class Predicates {
    // Part 1:
    static <A, B> Predicate<B> instanceOf(final Class<A> type) {
        return (B b) -> type.isAssignableFrom(b.getClass());
    }

    // Part 2:
    static <A> Predicate<A> isNull() {
        return (A a) -> a == null;
    }

    // Part 3:
    static <A> Predicate<A> isNotNull() {
        return Predicates.<A>isNull().not();
    }

    // Part 4:
    static <A> Predicate<A> alwaysTrue() {
        return a -> true;
    }

    // Part 5:
    static <A> Predicate<A> alwaysFalse() {
        return a -> false;
    }

    // Part 6:
    static <A> Predicate<A> isEqualTo(final A value) {
        return (A a) -> value.equals(a);
    }
}

@SuppressWarnings("unused")
class IntPredicates {
    // Part 7:
    static Predicate<Integer> isLargerThen(final Integer value) {
        return i -> i > value;
    }

    // Part 8:
    static Predicate<Integer> isLowerThen(final Integer value) {
        return i -> i < value;
    }
}

@SuppressWarnings("unused")
class UsingPredicates {
    // Part 9:
    static final Predicate<String> isNullOrEmpty =
            // Java has a limited type inference
            Predicates.<String>isNull().or(String::isEmpty);

    // Part 10:
    static final Predicate<Integer> isAllowed =
            isLargerThen(0).and(isLowerThen(6)).or(isEqualTo(42));
}

