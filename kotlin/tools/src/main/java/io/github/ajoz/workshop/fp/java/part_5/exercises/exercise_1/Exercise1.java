package io.github.ajoz.workshop.fp.java.part_5.exercises.exercise_1;

import io.github.ajoz.workshop.fp.java.tools.Function1;
import io.github.ajoz.workshop.fp.java.tools.control.Maybe;
import io.github.ajoz.workshop.fp.java.tools.control.Try;

/*
  -- Lifting --

  We played a lot with functions, especially in part 1 where we tried to compose
  everything. Of course this was only possible because we were creating instances
  of our very own "function" type called Function1. What about instance methods
  or static methods of classes?

  Java 8 introduced method references, it's possible to pass such a reference
  like `this::method1` or `SomeClass::method2`.

  It would be nice to use method references to produce instances of Function1,
  Function2, etc

  We already did this with a Supplier. For the supplier we created a silly
  method called `of(Supplier)`. This allowed us to feed it with a method reference
  and get back what we need because the compiler changed things on the fly.

  We can do the same for Function1

  interface Function1<A, B> {
      static <A, B> Function1<A, B> of(final Function1<A, B> function) {
          return function;
      }
  }

  This way we will be able to do:

  final Function1<SomeType, SomeType> func = Function1.of(SomeClass::method2);

  Why all the hassle?

  Because we would like to be able to compose stuff.

  It is not possible to do:

  (SomeClass::method2).andThen(someOtherFunction).andThen(someOtherFunction2)...

  Ok this is fixed, or is it?

  We can compose functions only if types line up. This is simple if we have two
  functions:
  f :: A -> B
  g :: B -> C

  We can compose them and create a new function:
  h = g(f)

  But what will happen if:

  f :: A -> Maybe<B>

  but `g` is still the same:

  g :: B -> C

  The types do not match anymore? That is a problem. We don't want to rework `g`
  maybe we do not have access to the sources and we just can't do it.

  We have two worlds of types here:
  - world of maybes
  - world of bare types

  |-----------------------------|
  | Maybe A, Maybe B, Maybe ... |
  |-----------------------------|



  |-----------------------------|
  | Types: A, B, ...            |
  |-----------------------------|

  It would help us a lot if our `g` function would work with Maybes instead of
  just types. If we could just "lift" it up from the world of types to the world
  of Maybes.

  So:

  g :: B -> C

  Would be:

  g' :: Maybe B -> Maybe C

  We already were doing this ;-) a few times in the course of the workshop.

  Part 1:

  Implement function `lift` that takes a one argument function as an argument
  and returns this function in the lifted form. Passing a Function1<A, B> should
  result with a Function1<Maybe<A>, Maybe<B>> returned.

  Hints:
  - let the types guide you!
  - what operation do you have to perform "inside"?
 */
@SuppressWarnings("unused")
final class Maybes {
    static <A, B> Function1<Maybe<A>, Maybe<B>> lift(final Function1<A, B> function) {
        throw new UnsupportedOperationException("Exercise 1 Maybes.lift is missing!");
    }
}

/*
  Part 2:

  Implement function `lift` that takes a one argument function as an argument
  and returns this function in the lifted form. Passing a Function1<A, B> should
  result with a Function1<Try<A>, Try<B>> returned.

  Hints:
  - similar to Maybe
 */
@SuppressWarnings("unused")
final class Trys {
    static <A, B> Function1<Try<A>, Try<B>> lift(final Function1<A, B> function) {
        throw new UnsupportedOperationException("Exercise 1 Maybes.lift is missing!");
    }
}

/*
  Now as the `lift` is implemented we can easily work within the world of Maybes
  or Trys, without changing our functions.

  Part 3:

  We have a special domain type called `Foo` this type is heavily used in our
  application. It has a special method called `getData` that returns a String
  but has a bad tendency of sometimes blowing up with a RuntimeException :-(

  The team that is responsible for `Foo` decided to deprecate it and then after
  sometime remove it completely. There will only be a method `tryGetData` left
  which means we need to lift all our API so it can work with it.

  Please look at `Foo` type and add static function `tryFooToBool` in the
  Exercise1 class.

  Hints:
  - use one of the newly created `lift` functions
  - use Function1.of
 */
@SuppressWarnings("unused")
class Foo {
    @Deprecated
    String getData() {
        return "unsafe (can blow up) Foo data!";
    }

    Try<String> tryGetData() {
        return Try.success("safe (can still blow up but we do not care) Foo data!");
    }
}

public class Exercise1 {
    // This is our Rich and Glorious API we cannot touch!!!
    private static Function1<String, Integer> strlen = String::length;
    private static Function1<Integer, Boolean> is42 = i -> i == 42;

    // This the original implementation:
    private static Function1<Foo, Boolean> fooToBool =
            Function1.of(Foo::getData)
                    .andThen(strlen)
                    .andThen(is42);

    // This is the new implementation you want to create through composition
    private static Function1<Foo, Try<Boolean>> tryFooToBool = foo -> {
        throw new UnsupportedOperationException("Exercise 1 tryFooToBool is missing!");
    };

    public static void main(final String[] args) {
        System.out.println("fooToBool.apply(new Foo()) = " + fooToBool.apply(new Foo()));
        System.out.println("tryFooToBool.apply(new Foo()) = " + tryFooToBool.apply(new Foo()));
    }
}
