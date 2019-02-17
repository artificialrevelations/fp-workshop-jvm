package io.github.ajoz.workshop.fp.java.part_2.solutions.exercise_6;

import io.github.ajoz.workshop.fp.java.tools.Function1;

import java.util.ArrayList;
import java.util.List;

class Exercise6 {
    // Part 1:
    static <A, B> Function1<B, Function1<List<A>, B>> foldLeftCurried(final Function1<B, Function1<A, B>> operator) {
        return (B initial) -> (List<A> list) -> {
            B accumulator = initial;
            for (final A element : list) {
                accumulator = operator.apply(accumulator).apply(element);
            }

            return accumulator;
        };
    }

    // Part 2:
    static Function1<List<Integer>, Integer> sum =
            foldLeftCurried((Integer a) -> (Integer b) -> a + b).apply(0);

    static Function1<List<Integer>, Integer> product =
            foldLeftCurried((Integer a) -> (Integer b) -> a + b).apply(1);

    // Part 3:
    static <A, B> Function1<List<A>, List<B>> mapCurried(final Function1<A, B> mapper) {
        return (List<A> list) -> {
            final List<B> mapped = new ArrayList<>(list.size());
            for (final A a : list) {
                mapped.add(mapper.apply(a));
            }
            return mapped;
        };
    }

    // Part 4:
    static Function1<List<String>, List<Integer>> lengths =
            mapCurried(String::length);
}
