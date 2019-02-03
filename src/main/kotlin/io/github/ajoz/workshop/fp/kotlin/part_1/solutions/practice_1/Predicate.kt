@file:Suppress("PackageName")

package io.github.ajoz.workshop.fp.kotlin.part_1.solutions.practice_1

// Part 1:
infix fun <A> ((A) -> Boolean).and(other: (A) -> Boolean): (A) -> Boolean =
        { value -> this(value) && other(value) }

// Part 2:
infix fun <A> ((A) -> Boolean).or(other: (A) -> Boolean): (A) -> Boolean =
        { value -> this(value) || other(value) }

// Part 3:
fun <A> ((A) -> Boolean).not(): (A) -> Boolean =
        { value -> !this(value) }

// Part 4:
fun <A> ((A) -> Boolean).xor(other: (A) -> Boolean): (A) -> Boolean {
    // could be: (this.or(other)).and((this.and(other).not()));
    // Less object creation the way below.
    return { value ->
        val p = this(value)
        val q = other(value)
        (p || q) && !(p && q)
    }
}