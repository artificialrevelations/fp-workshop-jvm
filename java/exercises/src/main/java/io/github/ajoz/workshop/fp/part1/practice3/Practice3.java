package io.github.ajoz.workshop.fp.part1.practice3;

import io.github.ajoz.workshop.fp.part1.practice1.Predicate;

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
     */
    public static <A> List<A> select(final Predicate<A> predicate,
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
    public static <A> List<A> reject(final Predicate<A> predicate,
                                     final List<A> elements) {
        throw new UnsupportedOperationException("Practice 3 Lists.reject is missing!");
    }
}

class ExercisesWithAList {
    
}

public class Practice3 {
}
