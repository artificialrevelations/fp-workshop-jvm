package io.github.ajoz.workshop.fp.java.part_1.exercises.exercise_4;

import kotlin.Pair;

/*
  -- Two argument Functions --

  part 1:

  Take a look at Function2 definition, is it similar to Function1?

  Question: Does a two argument function really exist?
  Question: How would a Function9 definition look like in Java?

  part 2:

  Create a function/method for converting a two argument function to a Function1
  - where arguments are expressed as a tuple
  - where return type is another Function1

  Question: Can a two argument function be composed in any meaningful way?
  Question: Why would we even need a Function2 if we can do everything with Function1?
  Question: How do you think, which representation is the most useful?

  part 3:

  Create a function/method for converting Function1 to Function2
  - from a Function1 where arguments are expressed as a tuple
  - from a Function1 where return type is another Function1

  part 4:

  Create a function/method called flip* that can flip argument order of a two
  argument function.
  - from a Function2<A, B, C> to Function2<B, A, C>
  - from a Function1<Pair<A, B>, C> to Function1<Pair<B, A>, C>
  - from a Function1<A, Function1<B, C>> to Function1<B, Function1<A, C>>
 */

// as Java does not have a concept of a tuple please use Pair class from kotlin stdlib!

@SuppressWarnings("unused")
@FunctionalInterface
interface Function1<A, B> {
    B apply(A a);
}

@FunctionalInterface
interface Function2<A, B, C> {
    C apply(A a, B b);
}

// as Java does not have a concept of a tuple please use Pair class from kotlin stdlib!

@SuppressWarnings("unused")
class Exercise4 {
    static <A, B, C> Function1<Pair<A, B>, C> tuple(final Function2<A, B, C> function2) {
        return (Pair<A, B> abPair) -> function2.apply(abPair.getFirst(), abPair.getSecond());
    }

    static <A, B, C> Function1<A, Function1<B, C>> curry(final Function2<A, B, C> function2) {
        return (A argA) -> (B argb) -> function2.apply(argA, argb);
    }

    static <A, B, C> Function2<A, B, C> untuple(final Function1<Pair<A, B>, C> function1) {
        return (a, b) -> function1.apply(new Pair<>(a, b));
    }

    static <A, B, C> Function2<A, B, C> uncurry(final Function1<A, Function1<B, C>> function1) {
        return (A argA, B argB) -> function1.apply(argA).apply(argB);
    }

    static <A, B, C> Function2<B, A, C> flip(final Function2<A, B, C> function2) {
        return (B argB, A argA) -> function2.apply(argA, argB);
    }

    static <A, B, C> Function1<Pair<B, A>, C> flipTupled(final Function1<Pair<A, B>, C> function1) {
        return (Pair<B, A> baPair) -> function1.apply(new Pair<>(baPair.getSecond(), baPair.getFirst()));
    }

    static <A, B, C> Function1<B, Function1<A, C>> flipCurried(final Function1<A, Function1<B, C>> function1) {
        return (B argB) -> (A arga) -> function1.apply(arga).apply(argB);
    }
}