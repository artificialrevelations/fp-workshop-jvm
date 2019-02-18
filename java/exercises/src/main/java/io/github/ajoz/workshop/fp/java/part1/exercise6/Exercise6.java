package io.github.ajoz.workshop.fp.java.part1.exercise6;

import io.github.ajoz.workshop.fp.java.tools.Function1;

/*
  -- Other types of Functions --

  In the Java 8 standard library you can find few additional "function-like"
  types:
  - Supplier<T> which is just a Function<Void, T>
  - Consumer<T> which is just a Function<T, Void>

  Why the fuss? Simply due to the peculiar nature of the Void type. This type
  before Java 5 (when generics were introduced) was used mainly in conjunction
  with reflection.

  Let's create instances of:
  - Function<Void, T>
  - Function<T, Void>

  and see what's working with them like
*/

@SuppressWarnings("unused")
class IssuesWithTheVoid {
    public static void main(final String[] args) {
        // Quirkiness of Java
        final Function1<Void, Integer> supplyInt = aVoid -> 42;

        // need to pass null as the Void constructor is private (looks strange!!)
        final Integer value = supplyInt.apply(null);

        final Function1<Integer, Void> consumeInt = integer -> {
            System.out.println(integer);
            // need to return null (looks ugly!!)
            return null;
        };

        consumeInt.apply(value);

        // composition:
        final Function1<Void, Void> blackHole =
                supplyInt.andThen(consumeInt);

        // we need to feed the black hole, maybe it will emit something ;-)
        final Void result = blackHole.apply(null);
    }
}

/*
  To ease the pain of using the Void type we will create similar types:
 */
@FunctionalInterface
interface Consumer1<A> {
    void accept(A a);
}

@FunctionalInterface
interface Supplier<A> {
    A get();
}

/*
  Are such "functions" useful? We talked about pure and impure functions, we also
  talked about referential transparency. So are pure, referential transparent
  functions like this useful?

  1) A pure referential transparent function from Void to T would need to return
  the same result when called each time and it would not be very interesting.

  A Random.nextInt() seems like a nice candidate, but it would have to return the
  same int each time.

  2) A pure referential transparent function from T to Void is even less interesting
  as the only "value" a Void type can take is null. So this function would eat up
  any input and return the same result.

  A System.out.println(T) seems like a nice candidate, but it should just eat up
  the argument and do nothing.

  That's the theory but in practice suppliers and consumers are used to model
  some impure effects like reading from a network resource or writing to a
  database. Such modeling of impure effects can be done better ofc.
 */
@SuppressWarnings("unused")
class Exercise6 {
    /*
      Part 1:

      Please create a function called `composeConsumer` that takes a one argument
      function from A to B and a consumer of B and returns as a result a consumer
      of A.
     */
    static <A, B> Consumer1<A> composeConsumer(final Function1<A, B> function,
                                               final Consumer1<B> consumer) {
        throw new UnsupportedOperationException("Exercise 6 composeConsumer is missing!");
    }

    /*
      Part 2:

      Please use the newly created function `composeConsumer` to compose
      - a consumer that prints Integers
      - a function that returns length of a String
     */
    static final class ComposingConsumer {
        public static void main(final String[] args) {
            final Consumer1<Integer> printInt = System.out::println;
            final Function1<String, Integer> strlen = String::length;

            final Consumer1<String> printStrLen =
                    string -> {
                        throw new UnsupportedOperationException("Exercise 6 printStrLen is missing!");
                    };

            printStrLen.accept("https://www.meetup.com/Java-User-Group-Lodz/");
        }
    }

    /*
      Part 3:

      Please create a function called `composeSupplier` that takes a supplier of
      A and a one argument function from A to B and returns a supplier of B.
     */
    static <A, B> Supplier<B> composeSupplier(final Supplier<A> supplier,
                                              final Function1<A, B> function) {
        throw new UnsupportedOperationException("Exercise 6 composeSupplier is missing!");
    }

    /*
      Part 4:

      Please use the newly created function `composeSupplier` to compose
      - a supplier that returns a String
      - a function that returns length of a String
     */
    static final class ComposingSupplier {
        public static void main(final String[] args) {
            final Supplier<String> getFacebook = () -> "https://www.facebook.com/groups/juglodz/";
            final Function1<String, Integer> strlen = String::length;

            final Supplier<Integer> getFBLen =
                    () -> {
                        throw new UnsupportedOperationException("Exercise 6 getFBLen is missing!");
                    };

            System.out.println(getFBLen.get());
        }
    }

    /*
      Part 5:

      Please create a function called `applyFirst` that takes a curried two
      argument function and uses a supplier to supply the first argument.
     */
    static <A, B, C> Function1<B, C> applyFirst(final Function1<A, Function1<B, C>> function,
                                                final Supplier<A> supplier) {
        throw new UnsupportedOperationException("Exercise 6 applyFirst is missing!");
    }

    /*
      Part 6:

      Please create a function called `applyFirst` that takes a curried two
      argument function and uses a supplier to supply the first argument.
     */
    static <A, B, C> Function1<A, C> applySecond(final Function1<A, Function1<B, C>> function,
                                                 final Supplier<B> supplier) {
        throw new UnsupportedOperationException("Exercise 6 applySecond is missing!");
    }
}
