package io.github.ajoz.workshop.fp.java.part_1.exercises.exercise_3;

/*
  -- Function Composition --

  Please test your solutions against available unit tests!

  Question: Does it matter in what order we will compose the functions?
  Question: How should the composeAll behave for one argument?
  Question: How should the composeAll behave for zero arguments?

  part 4:

  Enhance Function1 interface with two methods for composition.
  - andThen that first applies the function and then applies the one given as the argument
  - compose that first applies the function give as the argument and then applies the function
*/

@SuppressWarnings("unused")
class Exercise3 {
    /*
      Part 1:

      Create a function/method called composeIntFuns for composing two functions
      from Integer to Integer. The result of the composition should be the third
      function.
     */
    static Function1<Integer, Integer> composeIntFuns(final Function1<Integer, Integer> first,
                                                      final Function1<Integer, Integer> second) {
        throw new UnsupportedOperationException("Exercise3 composeIntFuns is missing!");
    }

    /*
      Part 2:

      Create a function/method called compose for composing two functions. One from
      type A to type B, second from type B to type C. The result of the composition
      should be a function from type A to type C.

      Hint:
      - Allow the types to guide you with the composition

      Questions:
      - Was there any other way to solve this exercise?
     */
    static <A, B, C> Function1<A, C> compose(final Function1<A, B> f,
                                             final Function1<B, C> g) {
        throw new UnsupportedOperationException("Exercise3 compose is missing!");
    }

    /*
      Part 3:

      Create a function called composeAll_1 for composing multiple functions
      passed as an argument. Solve this exercise by using function application.

      Hints:
      - the result of the previous function should be the argument of the next

      Questions:
      - Does it matter in what order we will compose the functions?
      - How should the composeAll_1 behave for one argument?
      - How should the composeAll_1 behave for zero arguments?
     */
    @SafeVarargs
    static <A> Function1<A, A> composeAll_1(final Function1<A, A>... functions) {
        throw new UnsupportedOperationException("Exercise3 composeAll_1 is missing!");
    }

    /*
      Part 4:

      Create a function called composeAll_2 for composing multiple functions
      passed as an argument. Solve this exercise by using the function
      compose(Function1, Function1).

      Hints:
      -
     */
    @SafeVarargs
    static <A> Function1<A, A> composeAll_2(final Function1<A, A>... functions) {
        throw new UnsupportedOperationException("Exercise3 composeAll_2 is missing!");
    }
}

@FunctionalInterface
interface Function1<A, B> {
    B apply(A a);

    @SuppressWarnings("unused")
    default <C> Function1<A, C> andThen(final Function1<B, C> after) {
        throw new UnsupportedOperationException("Exercise3 Function1.andThen is missing!");
    }

    @SuppressWarnings("unused")
    default <C> Function1<C, B> compose(final Function1<C, A> before) {
        throw new UnsupportedOperationException("Exercise3 Function1.compose is missing!");
    }
}

