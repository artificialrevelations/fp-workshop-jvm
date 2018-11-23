@file:Suppress("PackageName")

package io.github.ajoz.workshop.fp.kotlin.part_1.exercises.exercise_5

/*
  -- Other types of Functions --

  part 1:

  Java 8 introduced few additional "function" like types due to its peculiar
  type system.
  - Supplier<T> it replaces the need to have Function<Void, T>
  - Consumer<T> it replaces the need to have Function<T, Void>

  One can treat them as handy "aliases". In most cases they are used to model
  some impure effects like reading from a network resource or writing to a database.

  A pure referential transparent function from Void to T would need to return
  the same result when called each time and it would not be very interesting.

  A pure referential transparent function from T to Void is even less interesting
  as there is only value a Void type can take which is null.

  In Kotlin the type system is more "modern" and we can cleanly express the
  "supplier" and "consumer" with usual KFunction1 and Unit type.

  part 2:

  Create a function for composing a (A) -> B function and (B) -> Unit function.

  Question: Do we need any trickery? or special methods?

  part 3:

  Create a function for composing a (Unit) -> A function with a (A) -> B function.

  Question: The same as for part2, maybe we already have everything what we need?

  part 4:

  Create a function for partially applying the first argument of a curried
  function with the use of a Supplier.
 */

fun <A, B> kComposeConsumer(f: (A) -> B, c: (B) -> Unit): (A) -> Unit {
    TODO("Exercise5 kComposeConsumer is missing!")
}

fun <A, B> kComposeSupplier(f: (A) -> B, s: () -> A): () -> B {
    TODO("Exercise5 kComposeSupplier is missing!")
}

fun <A, B, C> kApplyFirst(f: (A) -> (B) -> C, s: () -> A): (B) -> C {
    TODO("Exercise5 kApplyFirst is missing!")
}