@file:Suppress("UNUSED_PARAMETER", "unused")

package io.github.ajoz.workshop.fp.part1.practice2

import io.github.ajoz.workshop.fp.tools.and
import io.github.ajoz.workshop.fp.tools.or
import kotlin.reflect.KClass

// Part 1:
fun <A : Any, B : Any> instanceOf(type: KClass<A>): (B) -> Boolean =
        { b: B -> type.isInstance(b) }

// Part 1 bonus:
inline fun <A, reified B> instanceOf(): (A) -> Boolean = { it is B }

// Part 2:
fun <A> alwaysTrue(): (A) -> Boolean = { true }

// Part 3:
fun <A> alwaysFalse(): (A) -> Boolean = { false }

// Part 4:
fun <A> isEqualTo(value: A): (A) -> Boolean = { it == value }

// Part 5:
fun isLargerThen(value: Int): (Int) -> Boolean = { it > value }

// Part 6:
fun isLowerThen(value: Int): (Int) -> Boolean = { it < value }

// Part 7:
val isAllowed: (Int) -> Boolean =
        (isLargerThen(0) and isLowerThen(6)) or isEqualTo(42)