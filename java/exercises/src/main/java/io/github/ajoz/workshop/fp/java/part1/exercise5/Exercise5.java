package io.github.ajoz.workshop.fp.java.part1.exercise5;

import io.github.ajoz.workshop.fp.java.tools.Function1;

/*
  -- Partial Application --

  From one point of view two argument functions are problematic because we always
  need to supply it with both arguments. Like you can imagine the problem escalates
  quickly with functions of larger arity.

  We already explored few ways of changing functions with arities greater then
  one to a one argument function.

  For the task at hand the most useful would be the curried form of a two or more
  argument function. Function of the form:

  Function1<A, Function1<B, C>>

  Can be partially applied. It means that the first argument can be supplied
  without passing the other one. As a result of passing the argument A we would
  get the result of Function1<B, C>

  Example:

  For example let's say you have a certain class that is responsible for loading
  some data let's call it a Loader, this class accepts instances of Foo and returns
  instances of Bar.

  interface Loader {
      // some class
      public Bar loadInfo(Foo foo) {
          // some heavy business logic
      }
  }

  Now somewhere in the code you are processing a List<Foo> and you need a List<Bar>
  That code expects a Function1<Foo, Bar>:

  public void doSomeMightyProcessing(final Function1<Foo, Bar> function) {

  We would like to use our Loader class and pass a correct Function1<Foo, Bar>
  to the `doSomeMightyProcessing`.

  Let's create a two argument curried Function:

  final Function1<Loader, Function1<Foo, Bar>> loadBarFromFoo =
        loader -> foo -> loader.loadInfo(foo);

  Now we can partially apply the function with a correct Loader instance

  final Function1<Foo, Bar> barToFoo = loadBarFromFoo.apply(someParticularLoader);
  doSomeMightyProcessing(barToFoo);
 */

@SuppressWarnings("unused")
public class Exercise5 {
    /*
      Part 1:

      Please create function `applyFirst` that takes a curried two argument
      function and the first argument. This method should return a one argument
      function as the result.
     */
    static <A, B, C> Function1<B, C> applyFirst(final Function1<A, Function1<B, C>> function,
                                                final A value) {
        throw new UnsupportedOperationException("Exercise 5 applyFirst is missing!");
    }

    /*
      Part 2:

      Please create function `applySecond` that takes a curried two argument
      function and the second argument. This method should return a one argument
      function as the result.
     */
    static <A, B, C> Function1<A, C> applySecond(final Function1<A, Function1<B, C>> function,
                                                 final B value) {
        throw new UnsupportedOperationException("Exercise 5 applySecond is missing!");
    }

    /*
      Part 3:

      Applying the first argument in a curried function is trivial. A bit less
      trivial in case of the second.
     */
    public static void main(final String[] args) {
        // Partially apply `addInts` function so you will get as a result
        // a function that always adds 1 to whatever it gets as an argument.
        final Function1<Integer, Function1<Integer, Integer>> addInts =
                a -> b -> a + b;

        final Function1<Integer, Integer> addOne =
                value -> {
                    throw new UnsupportedOperationException("Exercise 5 addOne is missing!");
                };

        System.out.println(addOne.apply(0));
        System.out.println(addOne.apply(1));
        System.out.println(addOne.apply(41));

        // Partially apply `concatStrings` so you will get as a result a function
        final Function1<String, Function1<String, String>> concatStrings =
                first -> second -> first + second;

        // that adds a "foo" prefix to any String argument
        final Function1<String, String> fooPrefix =
                value -> {
                    throw new UnsupportedOperationException("Exercise 5 fooPrefix is missing!");
                };

        // that adds a "bar" suffix to any String argument
        final Function1<String, String> barSuffix =
                value -> {
                    throw new UnsupportedOperationException("Exercise 5 fooPrefix is missing!");
                };

        System.out.println(fooPrefix.apply("rever with JUG Łódź!"));
        System.out.println(barSuffix.apply("Unfortunately no sponsors for an open "));
    }
}

@SuppressWarnings("unused")
interface Function2<A, B, C> {
    C apply(A a, B b);

    /*
      Part 4:

      Although we mentioned about partial application in the context of the
      one argument function. It can be achieved for a two argument function.
      Please add a method called `applyFirst` to the `Function2` interface.
      This method should return a one argument function as a result.
     */
    default Function1<B, C> applyFirst(final A value) {
        throw new UnsupportedOperationException("Exercise 5 applyFirst is missing!");
    }

    /*
      Part 5:

      Please add a method called `applySecond` to the `Function2` interface.
      This method should return a one argument function as a result.

      Questions:
      - Can Function2 be composed in any meaningful way?
     */
    default Function1<A, C> applySecond(final B value) {
        throw new UnsupportedOperationException("Exercise 5 applySecond is missing!");
    }

    /*
      Part 6:

      Please use the newly created functions `applyFirst` or `applySecond` to
      compose Function2 with Function1.
     */
    static void main(final String[] args) {
        // this function drops a certain amount of chars from a given string
        final Function2<String, Integer, String> drop =
                (string, amount) -> {
                    final int length = string.length();
                    if (amount < length)
                        return string.substring(amount, length);
                    else
                        return "";
                };

        final Function1<String, Integer> length = String::length;

        // We want to compose function `drop` with function `length`
        // We always want to drop 6 first characters from a given string

        final Function1<String, Integer> substrlen =
                string -> {
                    throw new UnsupportedOperationException("Exercise 5 substrlen is missing!");
                };

        System.out.println(substrlen.apply("This exercise is very easy?"));
    }
}
