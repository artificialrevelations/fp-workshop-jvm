@file:Suppress("PackageName")

package io.github.ajoz.workshop.fp.part_1.exercises.exercise_4

/*
  -- Two argument Functions --

  part 1:

  How does Kotlin allow to express a two argument function? What is the notation
  for it?

  Does a two argument function really exist?

  part 2:

  Create a function/method for converting a two argument function to a one argument
  function
  - where arguments are expressed as a tuple
  - where return type is another one argument function

  Question: Can a two argument function be composed in any meaningful way?
  Question: Why would we even need a two argument function if we can do
            everything with one argument function?
  Question: How do you think, which representation is the most useful?

  part 3:

  Create a function/method for converting a one argument function to a two
  argument function:
  - from a one argument function where arguments are expressed as a tuple
  - from a one argument function where return type is another one argument function
 */

fun <A, B, C> kConvertToFunction1WithPair(f: (A, B) -> C): (Pair<A, B>) -> C {
    TODO("KExercise4 kConvertToFunction1WithPair is missing!")
}

fun <A, B, C> kConvertToFunction1WithFunction(f: (A, B) -> C): (A) -> (B) -> C {
    TODO("KExercise4 kConvertToFunction1WithFunction is missing!")
}

fun <A, B, C> kConvertToFunction2FromPair(f: (Pair<A, B>) -> C): (A, B) -> C {
    TODO("KExercise4 kConvertToFunction2FromPair is missing!")
}

fun <A, B, C> convertToFunction2FromFunction(f: (A) -> (B) -> C): (A, B) -> C {
    TODO("KExercise4 kConvertToFunction2FromPair is missing!")
}