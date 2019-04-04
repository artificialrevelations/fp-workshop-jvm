package org.artrev.workshop.fp.tools

infix fun <A> ((A) -> Boolean).and(other: (A) -> Boolean): (A) -> Boolean =
        { value -> this(value) && other(value) }

infix fun <A> ((A) -> Boolean).or(other: (A) -> Boolean): (A) -> Boolean =
        { value -> this(value) || other(value) }

fun <A> ((A) -> Boolean).not(): (A) -> Boolean =
        { value -> !this(value) }

infix fun <A> ((A) -> Boolean).xor(other: (A) -> Boolean): (A) -> Boolean =
        { value ->
            val p = this(value)
            val q = other(value)
            (p || q) && !(p && q)
        }