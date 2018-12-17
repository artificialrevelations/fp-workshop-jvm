package io.github.ajoz.workshop.fp.java.part_5.exercises.exercise_2;

import io.github.ajoz.workshop.fp.java.tools.Function1;
import io.github.ajoz.workshop.fp.java.tools.Maybe;
import io.github.ajoz.workshop.fp.java.tools.Try;

import java.util.Collections;
import java.util.List;

/*
  -- Constraints Liberate, Liberties Constrain --

  "Most issues with programming comes from treating partial functions as total"

  Let's get back to the partial functions examples we were working with:

  public static Integer div(final Integer a, final Integer b) {
      return a / b;
  }

  It could throw an ArithmeticException if the argument `b` is 0

  public static <A> A head(final List<A> list) {
      return list.get(0);
  }

  The function `head` that returns a first element of the given `List` can throw
  and Exception if the given List is empty.

  How can we make those functions total?
  - we could modify their signatures to pass an argument with a default value,
    this default value would be use in case of any exception.
  - we could expand the codomain of the Function, we did this already by returning
    a Maybe or a Try type
  - we could restrict the domain of the Function. If `div` does not allow the `b`
    argument to be 0, why do we declare it as Integer? If `head` does not allow
    the given List to be empty, why do we pass a type of any possible list?

  In the previous exercise we were "lifting" a function to a world of Maybes and
  Trys. Could we "lift" a partial function to a world of total functions by
  extending it's codomain?

  Part 1:

  Please implement `liftP` function that takes a partial one argument function
  as an argument and returns a total function that returns a Maybe. Passing a
  Function1<A, B> should result with Function1<A, Maybe<B>> being returned.
 */

@SuppressWarnings("unused")
final class Maybes {
    static <A, B> Function1<A, Maybe<B>> liftP(final Function1<A, B> function) {
        throw new UnsupportedOperationException("Exercise 2 Maybes.liftP is missing!");
    }
}

/*
  Part 2:

  Please implement `liftP` function that takes a partial one argument function
  as an argument and returns a total function that returns a Maybe. Passing a
  Function1<A, B> should result with Function1<A, Try<B>> being returned.
 */
@SuppressWarnings("unused")
final class Trys {
    static <A, B> Function1<A, Try<B>> liftP(final Function1<A, B> function) {
        throw new UnsupportedOperationException("Exercise 2 Trys.liftP is missing!");
    }
}

/*
  When dealing with partial functions, we have to deal with them on our own or
  let someone else do it i.e. the functions caller.

  We can force them to deal with it in two ways:
  - forward - through the return type
  - backward - through the input argument type

  How to deal with partialness backward?

  Let's take our `div` function first:

  public static Integer div(final Integer a, final Integer b) {
      return a / b;
  }

  Instead of working with Integer maybe we could define a new type Natural.
  Natural numbers are all positive Integral numbers excluding Zero (at least that
  is how they thought me in school, ISO-8000002 defines Natural as positive Int
  and a Zero, but who believes in ISO?)

  public static Integer div(final Integer a, final Natural b) {
      return a / b;
  }

  What about the `head` function?

  public static <A> A head(final List<A> list) {
      return list.get();
  }

  We could define a NonEmptyList (NEL in short) and change our signature to:

  public static <A> A head(final NonEmptyList<A> list) {
      return list.get();
  }

  Questions:
  - Why don't we do that in Java?
  - Why aren't you doing this in your apps?

  This "backward" way can be used in different domains. Let's say we have an
  `Order` class in a Shop app. An `Order` has a List of Products.

  class Order {
      List<Product> products;
  }

  Is this true? Can we have an order with an empty List of products? Why do we
  allow representing an illegal state?

  class Traffic {
      List<Position> positions;
  }

  Can we have a Traffic information without an information about it's position?

  Make illegal states unrepresentable!!!!!!
 */
@SuppressWarnings("unused")
public class Exercise2 {
    public static Integer div(final Integer a, final Integer b) {
        return a / b;
    }

    public static <A> A head(final List<A> list) {
        return list.get(0);
    }

    public static void main(final String[] args) {
        final Function1<List<String>, Maybe<String>> maybeHead =
                Maybes.liftP(Exercise2::head);

        System.out.println("maybeHead.apply(emptyList) = " + maybeHead.apply(Collections.emptyList()));

        final Function1<List<String>, Try<String>> tryHead =
                Trys.liftP(Exercise2::head);

        System.out.println("tryHead.apply(emptyList) = " + tryHead.apply(Collections.emptyList()));
    }
}
