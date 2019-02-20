package io.github.ajoz.workshop.fp.part2.exercise3;

import io.github.ajoz.workshop.fp.tools.Function2;
import io.github.ajoz.workshop.fp.tools.collections.Lists;

import java.util.List;

class Exercise3 {
    // Part 1:
    static <A, B> B foldLeft(final List<A> list,
                             final B initial,
                             final Function2<B, A, B> operator) {
        B accumulator = initial;
        for (final A element : list) {
            accumulator = operator.apply(accumulator, element);
        }

        return accumulator;
    }

    // Part 2:
    static <A, B> B foldRight(final List<A> list,
                              final B initial,
                              final Function2<A, B, B> operator) {
        B accumulator = initial;
        for (int i = list.size(); i > 0; i--) {
            accumulator = operator.apply(list.get(i - 1), accumulator);
        }
        return accumulator;
    }

    // Part 3:
    static <A, B> B foldRight2(final List<A> list,
                               final B initial,
                               final Function2<A, B, B> operator) {
        return foldLeft(
                Lists.reverse(list),
                initial,
                operator.flip()
        );
    }
}
