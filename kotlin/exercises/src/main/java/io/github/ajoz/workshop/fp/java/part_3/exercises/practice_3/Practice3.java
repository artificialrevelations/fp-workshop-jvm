package io.github.ajoz.workshop.fp.java.part_3.exercises.practice_3;

/*
  -- More general sum types --

  In the exercises and practices we explored two useful sum types:
  - Maybe<A>
  - Try<A>

  First was used to express the possibility of non existing result. Second was
  used to express the possibility of a failing computation. If we look at those
  two types closely a something very obvious comes into the light.

  Both of those types model only simple two case types, that have distinct left
  and right "sides".

  In case of Maybe it was Some and None. In case of Try it was Success and Failure.

  Maybe we could try to create a type that will be either A or either B?

  data Maybe = None | Some
  data Try = Failure | Success
  data Either = Left | Right

  Please create a type called Either:
  - write `map` method that only works on the Right "side"
  - write `mapLeft` method that only works on the Left "side"
  - write `flatMap` method that only works on the Right "side"
  - write `onRight(Consumer1)` that takes a Consumer1 as an argument and runs the
    passed effect only on the Right "side", the `onRight` should return the
    Either as a result for operation chaining
  - write `onLeft(Consumer1)` should work similarly to `onRight` but for Left
    "side"
  - write `getRight` method that returns a value of Right "side" or throws an
    Exception if invoked on Left
  - write `getLeft` method that returns a value of Left "side" or throws an
    Exception if invoked on Right

  Questions:
  - Is it possible to change a Try to Either and back again without loosing any
    information?
  - Is it possible to change a Try to Maybe and back again without loosing any
    information?

 */
@SuppressWarnings("unused")
abstract class Either<A, B> {

}

public class Practice3 {
    public static void main(final String[] args) {
        // you can check how your implementation works here
    }
}