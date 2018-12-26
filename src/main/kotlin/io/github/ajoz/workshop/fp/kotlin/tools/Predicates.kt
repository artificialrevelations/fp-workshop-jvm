package io.github.ajoz.workshop.fp.kotlin.tools

// foo
infix fun <A> ((A) -> Boolean).and(other: (A) -> Boolean): (A) -> Boolean =
        { value -> this(value) && other(value) }

// bar
infix fun <A> ((A) -> Boolean).or(other: (A) -> Boolean): (A) -> Boolean =
        { value -> this(value) || other(value) }

fun <A> ((A) -> Boolean).not(): (A) -> Boolean =
        { value -> !this(value) }