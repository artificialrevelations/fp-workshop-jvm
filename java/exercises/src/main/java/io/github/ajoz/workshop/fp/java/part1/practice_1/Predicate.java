package io.github.ajoz.workshop.fp.java.part1.practice_1;

import io.github.ajoz.workshop.fp.java.tools.Function1;

/*
  -- Practice: Other type of functions --

  Java has access to both objects and primitive types. Unfortunately we cannot
  declare a Function1<A, boolean>. Java does not allow primitives used with
  generics if JEP218 comes to fruition we might get them.

  Until then we need a special type to declare such a function, Java 8 introduced
  it under the name of a "Predicate". It is a generic type from a generic type
  A to a boolean.

  Each Predicate returns a boolean this means that comparing to other functions
  we have a lot of different opportunities to "compose" them in some meaningful
  way.
 */
@SuppressWarnings("unused")
@FunctionalInterface
public interface Predicate<A> {
    boolean test(final A value);

    /*
      Part 1:

      Create a function `and` that composes two Predicates and returns another
      predicate. The resulting predicate should be based upon the return value
      of the both predicates used.
     */
    @SuppressWarnings("unused")
    default Predicate<A> and(final Predicate<A> other) {
        throw new UnsupportedOperationException("Practice 1 Predicate.and is missing!");
    }

    /*
      Part 2:

      Create a function `or` that composes two Predicates and returns another
      predicate. The resulting predicate should be based upon the return value
      of both predicates used.
     */
    default Predicate<A> or(final Predicate<A> other) {
        throw new UnsupportedOperationException("Practice 1 Predicate.or is missing!");
    }

    /*
      Part 3:

      Create a function `not` that returns a Predicate that is a negation.
     */
    default Predicate<A> not() {
        throw new UnsupportedOperationException("Practice 1 Predicate.not is missing!");
    }

    /*
      Part 4:

      Create a function `xor` (exclusive or), try to implement it only with the
      use of `and`, `or`, `not`.
     */
    default Predicate<A> xor(final Predicate<A> other) {
        throw new UnsupportedOperationException("Practice 1 Predicate.xor is missing!");
    }

    /*
      Part 5:

      Create a function `asFunction1` that converts the Predicate into a Function1.
     */
    default Function1<A, Boolean> asFunction1() {
        throw new UnsupportedOperationException("Practice 1 Predicate.asFunction1 is missing!");
    }
}

@SuppressWarnings({"Convert2MethodRef", "unused"})
class UsingPredicates {
    // Please read the predicates below and then follow with exercises:
    private static final Predicate<String> isNull = str -> str == null;
    private static final Predicate<String> isEmpty = str -> str.isEmpty();
    private static final Predicate<String> isBlank = str -> str.trim().isEmpty();

    /*
      Part 6:

      Please create a Predicate<String> called `isNullOrEmpty` that returns true
      if the given String is null or is empty (has no characters). Please use
      the existing predicates to solve this.

      Hints:
      - we can build predicates with and, or, xor
     */
    static final Predicate<String> isNullOrEmpty = str -> {
        throw new UnsupportedOperationException("Practice 1 isNullOrEmpty is missing!");
    };

    /*
      Part 7:

      Please create a Predicate<String> called `isNullOrBlank` that returns true
      if the given String is null or is blank (has only whitespace characters).
      Please use the existing predicates to solve this.

      Hints:
      - we can build predicates with and, or, xor
     */
    static final Predicate<String> isNullOrBlank = str -> {
        throw new UnsupportedOperationException("Practice 1 isNullOrBlank is missing!");
    };

    /*
      Part 8:

      Please create a Predicate<Integer> called `isAllowed` that returns true if
      the given Integer is between 0 and 6 or equal to 42. Please use the predicates
      defined below to solve this.
     */
    private static final Predicate<Integer> isLargerThen0 = i -> i > 0;
    private static final Predicate<Integer> isLowerThen6 = i -> i < 6;
    private static final Predicate<Integer> isEqualTo42 = i -> i == 42;

    static final Predicate<Integer> isAllowed = i -> {
        throw new UnsupportedOperationException("Practice 1 isAllowed is missing!");
    };
}