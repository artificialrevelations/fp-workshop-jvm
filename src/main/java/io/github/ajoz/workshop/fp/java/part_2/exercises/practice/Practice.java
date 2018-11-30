package io.github.ajoz.workshop.fp.java.part_2.exercises.practice;

import io.github.ajoz.workshop.fp.java.tools.Function1;
import io.github.ajoz.workshop.fp.java.tools.Predicate;

import java.util.List;

@SuppressWarnings("unused")
public class Practice {
    /*
      Part 1:

      Create a function called `forAll` that takes a List and a Predicate and
      checks if the predicate is satisfied for every element on the list.

      If list is empty a false should be returned

      Hints:
      - is this similar to the exercise what checks if the list contains an element?
      - what should be the initial value? true or false?
     */
    static <A> Boolean forAll(final List<A> list,
                              final Predicate<A> predicate) {
        if (list.isEmpty())
            return false;

        throw new UnsupportedOperationException("Practice forAll is missing!");
    }

    /*
      Part 2:

      Create a function called `map` that preforms mapping (like in the exercises)
      and write it in terms of `foldLeft`.

      Hints:
      - what should be the initial value for fold?
      - consider using Lists.append or Lists.prepend
      - when should the mapper function be called?
     */
    static <A, B> List<B> map(final List<A> list,
                              final Function1<A, B> mapper) {
        throw new UnsupportedOperationException("Practice map is missing!");
    }

    /*
      Part 3:

      Create a function called `composeAll` that returns a single function that
      is a result of composing every function given as the argument.

      Hints:
      - what should be the initial value? A function? Is there a function that is
        neutral for any composition?

      Questions:
      - what do you think should be passed if the List of argument functions is empty?
     */
    static <A> Function1<A, A> composeAll(final Function1<A, A>... functions) {
        throw new UnsupportedOperationException("Practice composeAll is missing!");
    }
}
