package org.artrev.workshop.fp.tools.predicates;

import org.artrev.workshop.fp.tools.Predicate;

public class IntPredicates {
    public static Predicate<Integer> isLargerThen(final Integer value) {
        return i -> i > value;
    }

    public static Predicate<Integer> isLowerThen(final Integer value) {
        return i -> i < value;
    }
}
