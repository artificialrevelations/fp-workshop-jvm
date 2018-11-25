package io.github.ajoz.workshop.fp.java.part_2.solutions.exercise_2;

import io.github.ajoz.workshop.fp.java.tools.Function2;

import java.util.List;

/*
  -- Advanced functions --

 */
class Exercise2 {
    /*
      This function `foo`

     */
    public static Integer foo(final List<Integer> list,
                              final Integer initial,
                              final Function2<Integer, Integer, Integer> operator) {
        Integer accumulator = initial;
        for (final Integer element : list) {
            accumulator = operator.apply(accumulator, element);
        }

        return accumulator;
    }

    /*


     */
}
