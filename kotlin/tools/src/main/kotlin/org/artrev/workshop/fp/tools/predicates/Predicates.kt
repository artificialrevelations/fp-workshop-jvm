package org.artrev.workshop.fp.tools.predicates

import kotlin.reflect.KClass

fun <A : Any, B : Any> instanceOf(type: KClass<A>): (B) -> Boolean =
        { b: B -> type.isInstance(b) }

inline fun <A, reified B> instanceOf(): (A) -> Boolean = { it is B }

fun <A> alwaysTrue(): (A) -> Boolean = { true }

fun <A> alwaysFalse(): (A) -> Boolean = { false }

fun <A> isEqualTo(value: A): (A) -> Boolean = { it == value }