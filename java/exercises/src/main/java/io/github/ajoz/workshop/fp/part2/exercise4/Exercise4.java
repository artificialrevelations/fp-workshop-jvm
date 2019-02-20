package io.github.ajoz.workshop.fp.part2.exercise4;

import java.util.List;

/*
  -- Power of fold --

  It is not immediately obvious but fold as a concept is very powerful. It is
  possible to express many different List functions in terms of fold.

  Please implement all the parts of this exercise with the usage of foldLeft
  or foldRight (your choice).
 */
@SuppressWarnings("unused")
class Exercise4 {

    /*
      Part 1:

      Create a function that finds a maximal Integer from the list.
      If the list is empty an Exception should be thrown.

      Hints:
      - What should be the initial value?
      - You can use Math.max function to solve this
     */
    static Integer maximum(final List<Integer> list) {
        if (list.isEmpty())
            throw new IllegalArgumentException("list is empty");

        throw new UnsupportedOperationException("Exercise 4 maximum is missing!");
    }

    /*
      Part 2:

      Create a function that finds a minimal integer from the list.
      If the list is empty an Exception should be thrown.

      Hints:
      - What should be the initial value?
      - You can use Math.min to solve this
     */
    static Integer minimum(final List<Integer> list) {
        if (list.isEmpty())
            throw new IllegalArgumentException("list is empty");

        throw new UnsupportedOperationException("Exercise 4 minimum is missing!");
    }

    /*
      Part 3:

      Create a function called `count` that returns the size of the list.

      Hints:
      - do not use List.size() method :-)
      - do we need the element for anything?
     */
    static <A> Integer count(final List<A> list) {
        throw new UnsupportedOperationException("Exercise 4 count is missing!");
    }

    /*
      Part 4:

      Create a function called `last` that returns the last element of the list.

      Hints:
      - do not use List.get(List.size() - 1) ;-)
      - what should be the first element? (check function Lists.head)
     */
    static <A> A last(final List<A> list) {
        throw new UnsupportedOperationException("Exercise 4 last is missing!");
    }

    /*
      Part 5:

      Create a function called `reverse` that returns the given list in the
      reverse order.

      Hints:
      - do not use Lists.reverse function ;-)
      - how should elements be processed? (check function Lists.prepend)
      - what should be the initial value for fold?
     */
    static <A> List<A> reverse(final List<A> list) {
        throw new UnsupportedOperationException("Exercise 4 reverse is missing!");
    }

    /*
      Part 6:

      Create a function called `average` that returns an average of the given
      list of Integers.

      Hints:
      - what should be returned if the list is empty?
      - is fold the only operation we need to do?
      - you can use Lists.sum if you do not want to write fold on your own
        but for exercise sake please try to restrain yourself to only using
        foldLeft and foldRight
     */
    static Integer average(final List<Integer> list) {
        throw new UnsupportedOperationException("Exercise 4 average is missing!");
    }

    /*
      Part 7:

      Create a function called `contains` that returns True if the given
      List contains the given searched Item, False otherwise.

      Hints:
      - what should be the initial value for fold?
      - what should be returned if the List is empty?
      - assume that the all the List elements have the `equals` method
        correctly implemented
     */
    static <A> Boolean contains(final List<A> list,
                                final A searched) {
        throw new UnsupportedOperationException("Exercise 4 contains is missing!");
    }

    /*
      Part 8:

      Create a function called `join` that returns a String representation
      of the given list, it should be possible to specify the separator
      used by the `join`.

      Hints:
      - if List is empty then an empty string should be returned
      - what should be returned if there is only one element in the list
      - how should we handle the join operation so the separator is only
        between the list elements. Can we use Lists.tail and Lists.head for
        help?
     */
    static <A> String join(final List<A> list,
                           final String separator) {
        if (list.isEmpty())
            return "";

        throw new UnsupportedOperationException("Exercise 4 join is missing!");
    }

    /*
      Part 9:

      Create a function called `penultimate` that returns the item next to the
      last item on the list.

      If the list is empty an Exception should be thrown.
      If the list has only one element and Exception should be thrown.

      Hint:
      - to make things easier elements of the list should be processed in pairs
      - what should be the starting pair?
      - think about using Lists.head and Lists.tail
     */
    static <A> A penultimate(final List<A> list) {
        if (list.isEmpty())
            throw new IllegalArgumentException("list is empty");

        if (list.size() == 1)
            throw new IllegalArgumentException("list has only one element");

        throw new UnsupportedOperationException("Exercise 4 penultimate is missing!");
    }
}
