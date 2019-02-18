package io.github.ajoz.workshop.fp.part2.exercise1;

import io.github.ajoz.workshop.fp.tools.Function2;

import java.util.List;

class Exercise1 {
    static Integer sum(final List<Integer> list) {
        Integer sum = 0;
        for (Integer value : list) {
            sum += value;
        }

        return sum;
    }

    static Integer product(final List<Integer> list) {
        Integer product = 1;
        for (Integer value : list) {
            product *= value;
        }

        return product;
    }

    static Integer foo(final List<Integer> list,
                       final Integer initial,
                       final Function2<Integer, Integer, Integer> operator) {
        Integer accumulator = initial;
        for (final Integer element : list) {
            accumulator = operator.apply(accumulator, element);
        }

        return accumulator;
    }

    // now let's rewrite sum in terms of foo
    static Integer fooSum(final List<Integer> list) {
        return foo(list, 0, (a, b) -> a + b);
    }

    // now let's rewrite product in terms of foo
    static Integer fooProduct(final List<Integer> list) {
        return foo(list, 1, (a, b) -> a * b);
    }
}
