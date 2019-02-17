@file:Suppress("PackageName")

package io.github.ajoz.workshop.fp.kotlin.part_1.solutions.exercise_2

// Part 1:
val f1: (Int) -> Int = { it + 1 }
val f2: (String) -> Int = { it.length }
val f3: (String) -> String = { "foo$it" }

// Part 2:
val str2int: (String) -> Int = { Integer.valueOf(it) }
val int2bool: (Int) -> Boolean = { it == 42 }
val str2bool: (String) -> Boolean = { int2bool(str2int(it)) }