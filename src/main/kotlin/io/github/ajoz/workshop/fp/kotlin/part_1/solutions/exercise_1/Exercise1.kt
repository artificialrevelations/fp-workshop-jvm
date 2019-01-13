@file:Suppress("PackageName", "PARAMETER_NAME_CHANGED_ON_OVERRIDE")

package io.github.ajoz.workshop.fp.kotlin.part_1.solutions.exercise_1

// Part 1:
// y = x + 1
val f1: (Int) -> Int = { x: Int -> x + 1 }

// y = (x + 2)^2
val f2: (Int) -> Int = { x -> (x + 2) * (x + 2) }

// y = -x + 10
val f3: (Int) -> Int = { x -> -x + 10 }

// y = x^2 + 4x + 1
val f4: (Int) -> Int = { x -> x * x + 4 * x + 1 }

/*
  Part 2:

  Create a function from String to Int that returns the size of a given String.
 */

val strlen: (String) -> Int = { str -> str.length }