@file:Suppress("PackageName")

package io.github.ajoz.workshop.fp.part_1.solutions.exercise_5

fun <A, B> kComposeConsumer(f: (A) -> B, c: (B) -> Unit): (A) -> Unit = { a: A ->
    c(f(a))
}

fun <A, B> kComposeSupplier(f: (A) -> B, s: () -> A): () -> B = {
    f(s())
}

fun <A, B, C> kApplyFirst(f: (A) -> (B) -> C, s: () -> A): (B) -> C = { b: B ->
    f(s())(b)
}