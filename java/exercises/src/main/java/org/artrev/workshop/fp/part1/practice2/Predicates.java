package org.artrev.workshop.fp.part1.practice2;

import org.artrev.workshop.fp.tools.Predicate;

/*
  -- Fun with Predicates vol. 1 --

  The Predicate<A> has a lot of useful methods that allow building more complex
  predicates, the only missing part is library of a general purpose predicates
  that can be used as building blocks.
 */
@SuppressWarnings({"unused", "SameParameterValue"})
class Predicates {
    /*
      Part 1:

      Please create a function called `instanceOf` that takes a Class<A> as an
      argument and returns a a Predicate<B>. This predicate should be able to
      check if the value is of the given type.

      Hints:
      - Use class.isAssignableFrom method
     */
    static <A, B> Predicate<B> instanceOf(final Class<A> type) {
        throw new UnsupportedOperationException("Practice 2 instanceOf is missing!");
    }

    /*
      Part 2:

      In the previous practice exercise we have created a predicate that was
      checking if a given String is null, now we want to create a function
      called `isNull` that returns a Predicate<A>. This predicate should be able
      to determine if the value it gets is null.

      Question:
      - Do we need to create a function `isNotNull` in the same way we created
        function `isNull`?
     */
    static <A> Predicate<A> isNull() {
        throw new UnsupportedOperationException("Practice 2 isNull is missing!");
    }

    /*
      Part 3:

      Please create a function `isNotNull`, please use `isNull` function to solve
      this exercise.

      Hints:
      - Which method defined on the Predicate type will be useful?
     */
    static <A> Predicate<A> isNotNull() {
        throw new UnsupportedOperationException("Practice 2 isNotNull is missing!");
    }

    /*
      Part 4:

      Please create a function `alwaysTrue` that returns a Predicate<A> that for
      any given value will return true.

      Hints:
      - There is not catch in this exercise!
     */
    static <A> Predicate<A> alwaysTrue() {
        throw new UnsupportedOperationException("Practice 2 alwaysTrue is missing!");
    }

    /*
      Part 5:

      Please create a function `alwaysFalse` that returns a Predicate<A> that for
      any given value will return false.

      Hints:
      - There is not catch in this exercise!
     */
    static <A> Predicate<A> alwaysFalse() {
        throw new UnsupportedOperationException("Practice 2 alwaysFalse is missing!");
    }

    /*
      Part 6:

      Please create a function called `isEqualTo` that returns a predicate that
      checks if its argument is equal to the given value.
    */
    static <A> Predicate<A> isEqualTo(final A value) {
        throw new UnsupportedOperationException("Practice 2 isEqualTo is missing!");
    }
}

@SuppressWarnings({"unused", "SameParameterValue"})
class IntPredicates {
    /*
      Part 7:

      Please create a function called `isLargerThen` that returns a predicate
      that checks if its argument is larger then the given value.
     */
    static Predicate<Integer> isLargerThen(final Integer value) {
        throw new UnsupportedOperationException("Practice 2 isLargerThen is missing!");
    }

    /*
      Part 8:

      Please create a function called `isLowerThen` that returns a predicate
      that checks if its argument is lower then the given value.
     */
    static Predicate<Integer> isLowerThen(final Integer value) {
        throw new UnsupportedOperationException("Practice 2 isLowerThen is missing!");
    }
}

@SuppressWarnings("unused")
class UsingPredicates {
    /*
      Part 9:

      Please create a Predicate<String> called `isNullOrEmpty`. To solve this
      exercise please use only already defined functions and methods.

      Hints:
      - use `isNull` function
      - use `Predicate.or` method
      - use `String::isEmpty` method reference
     */
    static final Predicate<String> isNullOrEmpty = str -> {
        throw new UnsupportedOperationException("Practice 2 isNullOrEmpty is missing!");
    };

    /*
      Part 10:

      Please create a Predicate<Integer> called `isAllowed` that returns true if
      the given Integer is between 0 and 6 or equal to 42. Please use the predicates
      defined below to solve this.
     */
    static final Predicate<Integer> isAllowed = i -> {
        throw new UnsupportedOperationException("Practice 2 isAllowed is missing!");
    };
}