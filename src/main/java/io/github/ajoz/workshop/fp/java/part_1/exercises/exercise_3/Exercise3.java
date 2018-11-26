package io.github.ajoz.workshop.fp.java.part_1.exercises.exercise_3;

/*
  -- Function Composition --

  Please test your solutions against available unit tests!

  part 1:

  Create a function/method called composeIntFuns for composing two functions
  from Integer to Integer. The result of the composition should be the third
  function.

  part 2:

  Create a function/method called compose for composing two functions. One from
  type A to type B, second from type B to type C. The result of the composition
  should be a function from type A to type C.

  part 3:

  Create a function/method called composeAll for composing multiple functions
  passed as an argument. Try to solve this part in several ways:
  - use the compose(Function1, Function1)
  - use function application

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
    // region part1
    static Function1<Integer, Integer> composeIntFuns(final Function1<Integer, Integer> first,
                                                      final Function1<Integer, Integer> second) {
        return (Integer argument) -> second.apply(first.apply(argument));
    }
    // endregion

    //region part2
    // hint: Allow the types to guide you with the composition
    static <A, B, C> Function1<A, C> compose(final Function1<A, B> f,
                                             final Function1<B, C> g) {
        return (A a) -> g.apply(f.apply(a));
    }
    //endregion

    //region part3
    @SafeVarargs
    static <A> Function1<A, A> composeAll(final Function1<A, A>... functions) {
        /*
        return (A arg) -> {
            A temp = arg;
            for (Function1<A, A> function : functions) {
                temp = function.apply(temp);
            }

            return temp;
        };
        */

        // second approach: identity function
        Function1<A, A> temp = x -> x;
        for (Function1<A, A> function : functions) {
            temp = compose(temp, function);
        }

        return temp;

    }
    //endregion
}

@FunctionalInterface
interface Function1<A, B> {
    B apply(A a);

    @SuppressWarnings("unused")
    default <C> Function1<A, C> andThen(final Function1<B, C> after) {
        return (A arg) -> after.apply(this.apply(arg));
    }

    @SuppressWarnings("unused")
    default <C> Function1<C, B> compose(final Function1<C, A> before) {
        return (C cArg) -> this.apply(before.apply(cArg));
    }
}

