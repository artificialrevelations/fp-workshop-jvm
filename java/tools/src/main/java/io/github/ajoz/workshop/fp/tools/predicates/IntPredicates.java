package io.github.ajoz.workshop.fp.tools.predicates;

import io.github.ajoz.workshop.fp.tools.Predicate;

public class IntPredicates {
    static Predicate<Integer> isLargerThen(final Integer value) {
        return i -> i > value;
    }

    static Predicate<Integer> isLowerThen(final Integer value) {
        return i -> i < value;
    }
}
