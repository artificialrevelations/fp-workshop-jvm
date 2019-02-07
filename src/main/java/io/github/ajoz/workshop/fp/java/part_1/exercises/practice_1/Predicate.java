package io.github.ajoz.workshop.fp.java.part_1.exercises.practice_1;

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
        throw new UnsupportedOperationException("Practice Predicate.and is missing!");
    }

    /*
      Part 2:

      Create a function `or` that composes two Predicates and returns another
      predicate. The resulting predicate should be based upon the return value
      of both predicates used.
     */
    default Predicate<A> or(final Predicate<A> other) {
        throw new UnsupportedOperationException("Practice Predicate.or is missing!");
    }

    /*
      Part 3:

      Create a function `not` that returns a Predicate that is a negation.
     */
    default Predicate<A> not() {
        throw new UnsupportedOperationException("Practice Predicate.not is missing!");
    }

    /*
      Part 4:

      Create a function `xor` (exclusive or), try to implement it only with the
      use of `and`, `or`, `not`.
     */
    default Predicate<A> xor(final Predicate<A> other) {
        throw new UnsupportedOperationException("Practice Predicate.xor is missing!");
    }

    /*
      Part 5:

      Create a function `asFunction1` that converts the Predicate into a Function1.
     */
    default Function1<A, Boolean> asFunction1() {
        throw new UnsupportedOperationException("Practice Predicate.asFunction1 is missing!");
    }
}