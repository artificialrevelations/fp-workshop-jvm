package io.github.ajoz.workshop.fp.java.part_2.solutions.exercise_5;

import io.github.ajoz.workshop.fp.java.tools.Function1;

import java.util.ArrayList;
import java.util.List;

public class Exercise5 {
    // Part 1:
    static List<Integer> mapInts(final List<Integer> list,
                                 final Function1<Integer, Integer> mapper) {
        final List<Integer> mapped = new ArrayList<>(list.size()); //root of all evil!
        for (final Integer value : list) {
            mapped.add(mapper.apply(value));
        }
        return mapped;
    }

    // Part 2:
    static <A, B> List<B> map(final List<A> list,
                              final Function1<A, B> mapper) {
        final List<B> mapped = new ArrayList<>(list.size()); //root of all evil!
        for (final A value : list) {
            mapped.add(mapper.apply(value));
        }
        return mapped;
    }

    // Part 3:
    static List<Integer> addOne(final List<Integer> list) {
        return map(list, integer -> integer + 1);
    }

    // Part 4:
    static List<Integer> lengths(final List<String> strings) {
        return map(strings, String::length);
    }
}
