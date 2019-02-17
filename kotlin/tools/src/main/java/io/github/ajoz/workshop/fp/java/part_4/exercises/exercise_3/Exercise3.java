package io.github.ajoz.workshop.fp.java.part_4.exercises.exercise_3;

/*
  -- Everything Memoized! --

  In the previous exercise we managed to memoize the `Supplier`, can we do the
  same with the `Function1` and `Function2` types?

  The interface `Supplier<A>` is just a `Function1<Void, A>` in disguise. Thus
  it should be easy to change all the `() -> A` into `A -> B`?
 */

/*
  Part 1:

  Please create two additional methods for the Function1:
  - default method called `memoized` that returns a one argument function that
    will store it's result value and return it each time on subsequent calls.
  - static method called `memoize` that takes a one argument function as an
    argument and returns its memoized version.

  Hints:
  - do not worry about the using the function between several threads
  - we need to store the value somehow (maybe mutation?)

  Questions:
  - what kind of functions pure or impure can be memoized?
  - why?
 */
@SuppressWarnings("unused")
interface Function1<A, B> {
    B apply(A arg);

    default Function1<A, B> memoized() {
        throw new UnsupportedOperationException("Exercise 3 Function1.memoized is missing!");
    }

    static <A, B> Function1<A, B> memoize(final Function1<A, B> function) {
        throw new UnsupportedOperationException("Exercise 3 Function1.memoize is missing!");
    }
}

/*
  Part 2:

  Please create two additional methods for the Function2:
  - default method called `memoized` that returns a two argument function that
    will store it's result value and return it each time on subsequent calls.
  - static method called `memoize` that takes a two argument function as an
    argument and returns its memoized version.

  Hints:
  - do not worry about the using the function between several threads
  - we need to store the value somehow (maybe mutation?)

  Questions:
  - what kind of functions pure or impure can be memoized?
  - why?
 */
@SuppressWarnings("unused")
interface Function2<A, B, C> {
    C apply(A arg1, B arg2);

    default Function2<A, B, C> memoized() {
        throw new UnsupportedOperationException("Exercise 3 Function2.memoized is missing!");
    }

    static <A, B, C> Function2<A, B, C> memoize(final Function2<A, B, C> function) {
        throw new UnsupportedOperationException("Exercise 3 Function2.memoize is missing!");
    }
}

public class Exercise3 {
    public static void main(final String[] args) {
        // Part 1:
        final Function1<String, Integer> fun1 =
                Function1.memoize(arg -> {
                    System.out.println(String.format("Argument passed: %s", arg));
                    return arg.length();
                });

        for (int i = 0; i < 10; i++) {
            System.out.println(fun1.apply("JUG Lodz"));
        }

        for (int i = 0; i < 10; i++) {
            System.out.println(fun1.apply("Mobilization Conference"));
        }

        // Part 2:
        final Function2<String, String, Integer> fun2 =
                Function2.memoize((arg1, arg2) -> {
                    System.out.println(String.format("Arguments passed: %s, %s", arg1, arg2));
                    return (arg1 + arg2).length();
                });

        for (int i = 0; i < 10; i++) {
            System.out.println(fun2.apply("JUG", "Lodz"));
        }

        for (int i = 0; i < 10; i++) {
            System.out.println(fun2.apply("Mobilization", "Conference"));
        }
    }
}
