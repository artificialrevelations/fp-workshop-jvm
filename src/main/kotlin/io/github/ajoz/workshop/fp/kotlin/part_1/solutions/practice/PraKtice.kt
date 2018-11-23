@file:Suppress("PackageName")

package io.github.ajoz.workshop.fp.kotlin.part_1.solutions.practice

// foo
infix fun <A> ((A) -> Boolean).and(other: (A) -> Boolean): (A) -> Boolean {
    return { value -> this(value) && other(value) }
}

// bar
infix fun <A> ((A) -> Boolean).or(other: (A) -> Boolean): (A) -> Boolean {
    return { value -> this(value) || other(value) }
}
