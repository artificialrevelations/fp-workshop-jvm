package io.github.ajoz.workshop.fp.java.part_3.exercises.practice;

import io.github.ajoz.workshop.fp.java.tools.Function1;

import java.util.NoSuchElementException;

/*
  -- Working effectively with failure --

  The sum type Maybe<A> we introduced in the exercises was very nice but it has
  a one major flaw. Although it allows indicating that there was an error, it
  eats the reason. There is no real way of communicating why the error occurred.

  Lets create a better type that will allow us to do that:

  data Try a = Success a | Failure Throwable

  A Try<A> is a sum type just like Maybe<A> but is capable of storing information
  about the exception (error situation) that caused the computation to fail.

  In this exercise create the type Try<A> with methods:
  - `map`
  - `flatMap`
  - `get`
  - `getOrElse`

  New methods:
  - `recoverWith` that takes a Function1<Throwable, Try<A>> and allows recovering
    from a failed computation. Think of it like "flatMap" but for Failure.
  - `recover` that takes a Function1<Throwable, A> and allows recovering from a
    failed computation. Think about it like a "map" but for Failure.
  - static `success` that takes a value and returns a Try.Success of this value
  - static `failure` that takes a Throwable and returns a Try.Failure of this
    error
 */
@SuppressWarnings({"unused", "WeakerAccess"})
abstract class Try<A> {

}

public class Practice {
    public static void main(final String[] args) {

    }
}
