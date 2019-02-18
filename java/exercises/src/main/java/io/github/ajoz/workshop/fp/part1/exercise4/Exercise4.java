package io.github.ajoz.workshop.fp.part1.exercise4;

/*
  -- Two argument Functions --

  Part 1:

  Take a look at the function definitions below:
 */
@FunctionalInterface
interface Function1<A, B> {
    B apply(A a);
}

// Does this type signature look nice?
@FunctionalInterface
interface Function2<A, B, C> {
    C apply(A a, B b);
}

// Does this type signature look that it is easy to work with it?
@SuppressWarnings("unused")
@FunctionalInterface
interface Function3<A, B, C, D> {
    D apply(A a, B b, C c);
}

// If Java limited type inference fails how nice it would be to manually specify
// all those types?
@SuppressWarnings("unused")
@FunctionalInterface
interface Function9<A, B, C, D, E, F, G, H, I, J> {
    J apply(A a, B b, C c, D d, E e, F f, G g, H h, I i);
}

/*
  Questions:
  - Does a Function2 or a Function3 (or a Function9 for that matter) even exists?
  - Do you see any problems with working with them?
  - Are there ways to make the code look more readable?

  Let's assume they do not exist and try to express them with only Function1. We
  can wrap every argument into a single type.

  So Function2 is really a Function1 that has a >> pair << passed as an argument.
  In a pseudo code you could write it as:

  Function2<A, B, C> == Function1<Pair<A, B>, C>

  The same thing happens for Function3:

  Function3<A, B, C, D> == Function1<Triple<A, B, C>, D>

  We need special types like Pair or Triple. Some languages have support for
  tuples but in java we need to make them on our own.

  In a JVM language called Ceylon each multi argument function is just a one
  argument function that takes a tuple as an argument. This makes supporting
  functions like Function2, Function3, ..., Function23 not necessary.
 */

/*
  Part 2:

  Create function called `tuple` that takes a Function2 as an argument and
  returns a Function1 as a result. As Java does not have native tuple support
  please use the Pair class defined below.
  */
class Pair<A, B> {
    final A first;
    final B second;

    Pair(A a, B b) {
        first = a;
        second = b;
    }
}

@SuppressWarnings("unused")
class Exercise4 {
    static <A, B, C> Function1<Pair<A, B>, C> tuple(final Function2<A, B, C> function2) {
        throw new UnsupportedOperationException("Exercise 4 tuple is missing!");
    }

    /*
      Part 3:

      Create function called `untuple` that takes a Function1 that has a Pair as
      the argument and returns a Function2 as a result.
     */
    static <A, B, C> Function2<A, B, C> untuple(final Function1<Pair<A, B>, C> function1) {
        throw new UnsupportedOperationException("Exercise 4 unTuple is missing!");
    }

    /*
      Part 4:

      Please read the functions below and use the newly created functions `tuple`
      and `untuple` to fix the example code.

      The suffixes added are:
      - UT - untupled
      - T - tupled
     */
    static final class TupleExample {
        static final Function2<String, String, String> addPrefixUT =
                (prefix, text) -> prefix + text;

        static final Function1<Pair<Integer, Integer>, Integer> addIntsT =
                pint -> pint.first + pint.second;

        public static void main(final String[] args) {
            final Function1<Pair<String, String>, String> addPrefixT =
                    p -> {
                        throw new UnsupportedOperationException("Exercise 4 addPrefixT is missing!");
                    };

            System.out.println(addPrefixT.apply(new Pair<>("https://", "nozama.com")));

            final Function2<Integer, Integer, Integer> addIntsUT =
                    (a, b) -> {
                        throw new UnsupportedOperationException("Exercise 4 addIntsUT is missing!");
                    };

            System.out.println(addIntsUT.apply(42, 0));
        }
    }

    /*
      Let's try to define Function2 in terms of Function1 again, this time without
      aiding ourselves with another class. Let's look at the Function2 definition
      again:

      Function2<A, B, C>

      If we can only pass a single argument, let's say the A what do we have:

      Function1<A, ???>

      If we are going to pass only A, we are missing B to compute the C, this
      means that we are missing part of the computation, this means that we could
      split the computation. We could split it in as many parts as there are
      arguments.

      Function2<A, B, C> == Function1<A, Function1<B, C>>

      Our Function3 would look like:

      Function3<A, B, C, D> == Function1<A, Function1<B, Function1<C, D>>>

      This method of expressing functions is called currying.

      In a CLR language called F# all functions are curried by default.
      */
    // Example:
    // Most verbose version
    @SuppressWarnings("Convert2Lambda")
    final Function1<String, Function1<Integer, String>> strPlusInt1 =
            new Function1<String, Function1<Integer, String>>() {
                @Override
                public Function1<Integer, String> apply(final String string) {
                    return new Function1<Integer, String>() {
                        @Override
                        public String apply(final Integer integer) {
                            return string + integer;
                        }
                    };
                }
            };

    // Less verbose version:
    @SuppressWarnings("CodeBlock2Expr")
    final Function1<String, Function1<Integer, String>> strPlusInt2 =
            (String string) -> {
                return (Integer integer) -> {
                    return string + integer; // we just concatenate String and Integer
                };
            };

    // Almost the minimal notation we can get in Java
    final Function1<String, Function1<Integer, String>> strPlusInt3 =
            (String string) -> (Integer integer) -> string + integer;

    // Absolute minimal notation we can get in Java
    final Function1<String, Function1<Integer, String>> strPlusInt4 =
            string -> integer -> string + integer;

    /*
      Part 5:

      Create function called `curry` that takes a Function2 as an argument and
      returns its curried representation.
   */
    static <A, B, C> Function1<A, Function1<B, C>> curry(final Function2<A, B, C> function2) {
        throw new UnsupportedOperationException("Exercise 4 curry is missing!");
    }

    /*
      Part 6:

      Create function called `uncurry` that takes a curried two argument function
      and returns its Function2 representation.
     */
    static <A, B, C> Function2<A, B, C> uncurry(final Function1<A, Function1<B, C>> function1) {
        throw new UnsupportedOperationException("Exercise 4 uncurry is missing!");
    }

    /*
      Part 7:

      Please read the functions below and use the newly created functions `curry`
      and `uncurry` to fix the example code.

      The suffixes added are:
      - UC - uncurried
      - C - curried
     */
    static final class CurryExample {
        public static void main(final String[] args) {
            // curried
            // simple int multiplication
            final Function1<Integer, Function1<Integer, Integer>> multiplyC =
                    a -> {
                        throw new UnsupportedOperationException("Exercise 4 multiplyC is missing!");
                    };

            // use `uncurry` function to solve this
            final Function2<Integer, Integer, Integer> multiplyUC =
                    (a, b) -> {
                        throw new UnsupportedOperationException("Exercise 4 multiplyUC is missing!");
                    };

            System.out.println(multiplyUC.apply(42, 1));

            // uncurried:
            // repeat the same text specified number of times
            final Function2<Integer, String, String> replicateUC =
                    (times, str) -> {
                        final StringBuilder result = new StringBuilder();
                        for (int i = 0; i < times; i++) {
                            result.append(str);
                        }
                        return result.toString();
                    };

            final Function1<Integer, Function1<String, String>> replicateC =
                    times -> {
                        throw new UnsupportedOperationException("Exercise 4 replicateC is missing");
                    };

            System.out.println(replicateC.apply(42).apply("JUG"));
        }
    }

    /*
      Part 8:

      Useful functions. Sometimes we are working with an API that does accept
      functions of certain signature. This can be problematic if our own API
      has slightly different signature i.e. order or arguments is different.

      Create function `flip` that takes a Function2 as an argument and returns
      another Function2 but with the arguments flipped. In case of Function2
      first arg becomes second, second becomes first.
     */
    static <A, B, C> Function2<B, A, C> flip(final Function2<A, B, C> function2) {
        throw new UnsupportedOperationException("Exercise 4 flip is missing!");
    }

    /*
      Part 9:

      Create function `flipTupled` that takes a Function1 with a pair argument
      and returns another Function1 with a pair argument, this time the order of
      arguments in the pair should be switched.
     */
    static <A, B, C> Function1<Pair<B, A>, C> flipTupled(final Function1<Pair<A, B>, C> function1) {
        throw new UnsupportedOperationException("Exercise 4 flipTupled is missing!");
    }

    /*
      Part 10:

      Create function `flipCurried` that takes a curried function and returns a
      curried function with the argument switched. A function a -> b -> c after
      flipCurried should be b -> a -> c
     */
    static <A, B, C> Function1<B, Function1<A, C>> flipCurried(final Function1<A, Function1<B, C>> function1) {
        throw new UnsupportedOperationException("Exercise 4 flipCurried is missing!");
    }
}