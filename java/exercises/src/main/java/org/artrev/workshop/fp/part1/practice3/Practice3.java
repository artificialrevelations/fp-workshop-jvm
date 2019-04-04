package org.artrev.workshop.fp.part1.practice3;

import org.artrev.workshop.fp.tools.Function1;
import org.artrev.workshop.fp.tools.Predicate;

import java.util.List;

/*
  -- Fun with Predicates vol. 2 --

  We've built the Predicate type, we've built a nice collection of general
  purpose predicates but what about using them?
 */
class Lists {
    /*
      Part 1:

      Please create a function called `select` that takes a Predicate and a List
      of elements and returns another List that has only elements that satisfy
      the given predicate.

      Hints:
      - you can use a simple iteration to write this function!

      Questions:
      - if you ever used Java Stream API or RxJava then probably you used a
      similar function, in most cases it was called `filter`, why do you think
      we called it `select`?
      - in most known Java/Kotlin APIs the input List (collection) is the first
      argument, why do you think we decided to put it as the last?
      - is putting the argument last worth it?
     */
    static <A> List<A> select(final Predicate<A> predicate,
                              final List<A> elements) {
        throw new UnsupportedOperationException("Practice 3 Lists.select is missing!");
    }

    /*
      Part 2:

      Please create a function called `reject` that takes a Predicate and a List
      of elements and returns another List that has only elements that do NOT
      satisfy the given predicate.

      Knowing the methods implemented over the Predicate type can you implement
      the `reject` in terms of `select`?
     */
    static <A> List<A> reject(final Predicate<A> predicate,
                              final List<A> elements) {
        throw new UnsupportedOperationException("Practice 3 Lists.reject is missing!");
    }
}

class ExercisesWithAList {
    /*
      Part 3:

      Please create a function called `allAbove42` that takes a List of integers
      and returns another List that contains only values that are above 42. Try
      to solve this exercise two times: first with the use of `select`, second
      with the use of `reject`.

      Hints:
      - In the tools module there is a `predicates` package with an IntPredicates
     */
    static List<Integer> allAbove42(final List<Integer> values) {
        throw new UnsupportedOperationException("Practice 3 allAbove42 is missing!");
    }

    /*
      Part 4:

      Please create a function called `allBelow42` that takes a List of integers
      and returns another List that contains only values that are below 42.

      Try using currying and partial application to solve this exercise.

      Hints:
      - In the tools module there is a `Function2` class that has a static method
        `curry`. You can pass reference to Lists.select there

      Questions:
      - Are there problems with implementing this exercise?
      - Do you like how the code is written?
     */
    static Function1<List<Integer>, List<Integer>> allBelow42 = ints -> {
        throw new UnsupportedOperationException("Practice 3 allBelow42 is missing!");
    };
}

/*
  Part 5:

  Please implement the predicates below and then by using them and of your choice
  `Lists.select` or `Lists.reject` create a function `getStrings` that takes a
  List of Strings and returns another List with Strings that only conform to the
  rules listed here:
  - String should have a first OR last capital letter
  - String should have length larger then 3 AND smaller then 10
  - String should not be null OR empty
  - String should contain the word "JUG"

  You can use any of the general purpose predicates from the tools module that
  you need.

  Hints:
  - use Character.isUpperCase to check if the letter is a capital letter
  - you can use predicates that are defined in tools module
 */
@SuppressWarnings("unused")
class Practice3 {
    private static Predicate<String> hasFirstCapitalLetter = str -> {
        throw new UnsupportedOperationException("Practice 3 hasFirstCapitalLetter is missing!");
    };

    private static Predicate<String> hasLastCapitalLetter = str -> {
        throw new UnsupportedOperationException("Practice 3 hasLastCapitalLetter is missing!");
    };

    private static Predicate<String> hasLengthLargerThen(final int length) {
        throw new UnsupportedOperationException("Practice 3 hasLengthLargerThen is missing!");
    }

    private static Predicate<String> hasLengthSmallerThen(final int length) {
        throw new UnsupportedOperationException("Practice 3 hasLengthSmallerThen is missing!");
    }

    private static Predicate<String> contains(final String text) {
        throw new UnsupportedOperationException("Practice 3 contains is missing!");
    }

    static List<String> getStrings(final List<String> data) {
        throw new UnsupportedOperationException("Practice 3 getStrings is missing!");
    }
}
