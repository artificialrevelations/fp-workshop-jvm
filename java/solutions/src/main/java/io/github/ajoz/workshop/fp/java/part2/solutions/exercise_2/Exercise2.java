package io.github.ajoz.workshop.fp.java.part2.solutions.exercise_2;

import io.github.ajoz.workshop.fp.java.tools.Function2;

import java.util.List;

class Exercise2 {
    // Part 1:
    static Integer foo(final List<Integer> list,
                       final Integer initial,
                       final Function2<Integer, Integer, Integer> operator) {
        Integer accumulator = initial;
        for (final Integer element : list) {
            accumulator = operator.apply(accumulator, element);
        }

        return accumulator;
    }

    // Part 2:
    static <A, B> B bar(final List<A> list,
                        final B initial,
                        final Function2<B, A, B> operator) {
        B accumulator = initial;
        for (final A element : list) {
            accumulator = operator.apply(accumulator, element);
        }

        return accumulator;
    }

    // Part 3:
    static Integer sum(final List<Integer> list) {
        return bar(list, 0, (a, b) -> a + b);
    }

    static Integer product(final List<Integer> list) {
        return bar(list, 1, (a, b) -> a * b);
    }
}
