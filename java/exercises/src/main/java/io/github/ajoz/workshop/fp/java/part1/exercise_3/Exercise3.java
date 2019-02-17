package io.github.ajoz.workshop.fp.java.part1.exercise_3;

import java.util.Arrays;
import java.util.List;

/*
  -- Function Composition --

  Please test your solutions against available unit tests!
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
      - Can you think of a function that composed with any other function does
        not change the result?
     */
    @SafeVarargs
    static <A> Function1<A, A> composeAll_2(final Function1<A, A>... functions) {
        throw new UnsupportedOperationException("Exercise3 composeAll_2 is missing!");
    }
}

@FunctionalInterface
interface Function1<A, B> {
    B apply(A a);

    /*
      Part 5:

      Add a method called `andThen` to the `Function1` interface. It should compose
      function on which it is called with the function that is passed to it as
      an argument. The resulting function should first apply the given value
      and then it should apply the result to the function that was passed as the
      `andThen` argument.

      Example:
      final Function1<String, String> fooify = string -> "foo" + string;
      final Function1<String, Integer> length = string -> string.length();

      // composition:
      final Function1<String, Integer> fooifiedLength = fooify.andThen(length);
     */
    @SuppressWarnings("unused")
    default <C> Function1<A, C> andThen(final Function1<B, C> after) {
        throw new UnsupportedOperationException("Exercise3 Function1.andThen is missing!");
    }

    /*
      Part 6:

      Add a method called `compose` to the `Function1` interface. It should compose
      function of which it is called with the function that is passe to it as an
      argument. The resulting function should first apply the given value to
      the function that was passed as the `compose` argument and then apply the
      result.

      Example:
      final Function1<String, String> fooify = string -> "foo" + string;
      final Function1<String, Integer> length = string -> string.length();

      // composition:
      final Function1<String, Integer> fooifiedLength = length.compose(fooify);
     */
    @SuppressWarnings("unused")
    default <C> Function1<C, B> compose(final Function1<C, A> before) {
        throw new UnsupportedOperationException("Exercise3 Function1.compose is missing!");
    }
}

@SuppressWarnings("unused")
class Example {
    /*
      Part 7:

      You are working for an online shopping company Nozama, you need to create
      a code that will produce urls for particular products that you have available.

      The Product is a simple class containing id and a name:
     */
    static class Product {
        public Id id;
        Description description;
        // other fields

        Product(final Id id,
                final Description description) {
            this.id = id;
            this.description = description;
        }
    }

    static class Id {
        public String value;

        Id(final String value) {
            this.value = value;
        }
    }

    static class Description {
        public String name;

        Description(final String name) {
            this.name = name;
        }
    }

    /*
      You already have a few functions available:

      - getProductId that takes a Product and returns its Id
      - getPurchaseUri that takes an Id and returns a String uri
      - getSecureUrl that takes an uri and returns a secure url that is using https
     */

    static final Function1<Product, Id> getProductId =
            product -> product.id;

    static final Function1<Id, String> getPurchaseUri =
            productId -> "nozama.com/shop/purchase?=" + productId.value;

    static final Function1<String, String> getSecureUrl =
            uri -> "https://" + uri;

    /*
      Compose those three functions together to get a third function:
     */
    static final Function1<Product, String> getSecureProductPurchaseUrl =
            product -> {
                throw new UnsupportedOperationException("Exercise 3 getSecureProductPurchaseUrl is missing!");
            };

    public static void main(final String[] args) {
        // You can play here with the code:

        final List<Product> products =
                Arrays.asList(
                        new Product(
                                new Id("JUGLDZ42JAVA"),
                                new Description("Workshop: FP in Java")
                        ),
                        new Product(
                                new Id("JUGLDZ42KOTLIN"),
                                new Description("Workshop: FP in Kotlin")
                        )
                );

        /*
          As you can clearly see, this approach gives us a very high reusability
          as we can mix and match our "lego" pieces to create new business logic.
         */
        for (final Product product : products) {
            System.out.println(getSecureProductPurchaseUrl.apply(product));
        }
    }
}

