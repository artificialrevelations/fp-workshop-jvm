@file:Suppress("PackageName")

package io.github.ajoz.workshop.fp.kotlin.part_1.exercises.exercise_4

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

  part 4:

  Create a function/method called flip* that can flip argument order of a two
  argument function.
  - from a (A, B) -> C to (B, A) -> C
  - from a (Pair<A, B>) -> C to (Pair<B, A>) -> C
  - from a (A) -> (B) -> C to (B) -> (A) -> C
 */

fun <A, B, C> kTuple(f: (A, B) -> C): (Pair<A, B>) -> C {
    TODO("KExercise4 kTuple is missing!")
}

fun <A, B, C> kCurry(f: (A, B) -> C): (A) -> (B) -> C {
    TODO("KExercise4 kCurry is missing!")
}

fun <A, B, C> kUnTuple(f: (Pair<A, B>) -> C): (A, B) -> C {
    TODO("KExercise4 kUnTuple is missing!")
}

fun <A, B, C> kUnCurry(f: (A) -> (B) -> C): (A, B) -> C {
    TODO("KExercise4 kUnCurry is missing!")
}

fun <A, B, C> kFlip(f: (A, B) -> C): (B, A) -> C {
    TODO("KExercise4 kFlip is missing!")
}

fun <A, B, C> kFlipTupled(f: (Pair<A, B>) -> C): (Pair<B, A>) -> C {
    TODO("KExercise4 kFlipTupled is missing!")
}

fun <A, B, C> kFlipCurried(f: (A) -> (B) -> C): (B) -> (A) -> C {
    TODO("KExercise4 kFlipCurried is missing!")
}


