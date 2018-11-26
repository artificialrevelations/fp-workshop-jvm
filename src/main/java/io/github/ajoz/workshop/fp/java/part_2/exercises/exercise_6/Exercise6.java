package io.github.ajoz.workshop.fp.java.part_2.exercises.exercise_6;

import io.github.ajoz.workshop.fp.java.tools.Function1;

import java.util.List;

class Exercise6 {
    /*
       Part 1:

       Create a function called `foldLeftCurried` that perform a foldLeft but
       in curried form.
      */
    static <A, B> Function1<B, Function1<List<A>, B>> foldLeftCurried(final Function1<B, Function1<A, B>> operator) {
        throw new UnsupportedOperationException("Exercise 6 foldLeftCurried is missing!");
    }

    /*
       Part 2:

       Create a function called `sum` that is using `foldLeftCurried`
       Create a function called `product` that is using `foldLeftCurried`
      */

    /*
    static Function1<List<Integer>, Integer> sum =

    static Function1<List<Integer>, Integer> product =
      */

    /*
      Part 3:

      Create a function called `mapCurried` that perform a map but in curried form.
     */
    static <A, B> Function1<List<A>, List<B>> mapCurried(final Function1<A, B> mapper) {
        throw new UnsupportedOperationException("Exercise 6 mapCurried is missing!");
    }

    /*
       Part 4:

       Create a function called `lengths` that is using `mapCurried`.
      */

    /*
    static Function1<List<String>, List<Integer>> lengths =
     */
}
