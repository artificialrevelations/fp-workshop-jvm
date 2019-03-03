package io.github.ajoz.workshop.fp.part1.practice3;

import io.github.ajoz.workshop.fp.tools.Function1;
import io.github.ajoz.workshop.fp.tools.Function2;
import io.github.ajoz.workshop.fp.tools.Predicate;
import io.github.ajoz.workshop.fp.tools.predicates.IntPredicates;
import io.github.ajoz.workshop.fp.tools.predicates.Predicates;

import java.util.ArrayList;
import java.util.List;

class Lists {
    static <A> List<A> select(final Predicate<A> predicate,
                              final List<A> elements) {
        final List<A> selected = new ArrayList<>();
        for (final A element : elements) {
            if (predicate.test(element))
                selected.add(element);
        }
        return selected;
    }

    static <A> List<A> reject(final Predicate<A> predicate,
                              final List<A> elements) {
        return Lists.select(predicate.not(), elements);
    }
}

class ExercisesWithAList {
    static List<Integer> allAbove42(final List<Integer> values) {
        return Lists.select(IntPredicates.isLargerThen(42), values);
    }

    static Function1<List<Integer>, List<Integer>> allBelow42 =
            /*
               Unfortunately javac cannot correctly infer the types of the
               expression and we need to help him by marking the types for the
               Function2.curry method.
              */
            Function2.<Predicate<Integer>, List<Integer>, List<Integer>>curry(Lists::select).apply(IntPredicates.isLowerThen(42));
    // it does not look good or appealing :-( such is the cruel fate of
    // point free FP in Java

}

class Practice3 {
    private static Predicate<String> hasFirstCapitalLetter =
            str -> Character.isUpperCase(str.charAt(0));

    private static Predicate<String> hasLastCapitalLetter =
            str -> Character.isUpperCase(str.charAt(str.length() - 1));

    private static Predicate<String> hasLengthLargerThen(final int length) {
        return str -> str.length() > length;
    }

    private static Predicate<String> hasLenghtSmallerThen(final int length) {
        return str -> str.length() < length;
    }

    private static Predicate<String> contains(final String text) {
        return str -> str.contains(text);
    }

    private static Predicate<String> hasLengthInRange =
            hasLengthLargerThen(3).and(hasLenghtSmallerThen(10));

    private static Predicate<String> isNotNull = Predicates.isNotNull();

    private static Predicate<String> hasCorrectFirstOrLastLetter =
            hasFirstCapitalLetter.or(hasLastCapitalLetter);

    static List<String> getStrings(final List<String> data) {
        return Lists.select(
                isNotNull.and(hasLengthInRange).and(hasCorrectFirstOrLastLetter).and(contains("JUG")),
                data
        );
    }
}
