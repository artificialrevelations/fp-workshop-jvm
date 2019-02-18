package io.github.ajoz.workshop.fp.java.part2.exercise5;

import io.github.ajoz.workshop.fp.java.tools.Function1;

import java.util.List;

/*
  -- Other useful functions --

  The world of higher order functions does not end on fold. There are many
  other useful function that are begging to be written and used.
 */
public class Exercise5 {
    /*
      Part 1:

      Create a function called `mapInts` that takes a list of Integers as an
      argument and a one argument function. This `mapInts` should return a List
      of Integers that are result of applying the given function.

      Hints:
      - new list should have the same size and old list
      - for empty list an empty list should be returned
      - mapInts([1, 2, 3], x -> x + 1) == [2, 3, 4]
     */
    static List<Integer> mapInts(final List<Integer> list,
                                 final Function1<Integer, Integer> mapper) {
        throw new UnsupportedOperationException("Exercise 5 mapInts is missing!");
    }

    /*
      Part 2:

      Create a function called `map` that is a generalization of `mapInts`.
      It should take a List of elements of type A and a one argument function.
      It should be possible to express `mapInts` with this new `map`

      Hints:
      - new list should have the same size and old list
      - for empty list an empty list should be returned
      - let the types guide you
      */
    static <A, B> List<B> map(final List<A> list,
                              final Function1<A, B> mapper) {
        throw new UnsupportedOperationException("Exercise 5 map is missing!");
    }

    /*
      Part 3:

      Create a function called `addOne` that takes a List of Integers and returns
      a List of Integers increased by one. This `addOne` should use the `map`
      function.

      Hints:
      - new list should have the same size and old list
      - for empty list an empty list should be returned
      */
    static List<Integer> addOne(final List<Integer> list) {
        throw new UnsupportedOperationException("Exercise 5 addOne is missing!");
    }

    /*
      Part 4:

      Create a function called `lengths` that takes a List of Strings and returns
      a List of Integers (lengths of those strings). This `lengths` should use
      the `map` function.

      Hints:
      - new list should have the same size and old list
      - for empty list an empty list should be returned
      - you can use String::length
     */
    static List<Integer> lengths(final List<String> strings) {
        throw new UnsupportedOperationException("Exercise 5 lengths is missing!");
    }
}
