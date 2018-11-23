@file:Suppress("PackageName")

package io.github.ajoz.workshop.fp.kotlin.part_1.solutions.exercise_4

val example2ArgFunction: (Int, Int) -> Int = { a: Int, b: Int ->
    a + b
}

fun <A, B, C> kTuple(f: (A, B) -> C): (Pair<A, B>) -> C = { pair ->
    f(pair.first, pair.second)
}

fun <A, B, C> kCurry(f: (A, B) -> C): (A) -> (B) -> C = { a: A ->
    { b: B ->
        f(a, b)

    }
}

fun <A, B, C> kUnTuple(f: (Pair<A, B>) -> C): (A, B) -> C = { a: A, b: B ->
    f(Pair(a, b))
}

fun <A, B, C> kUnCurry(f: (A) -> (B) -> C): (A, B) -> C = { a: A, b: B ->
    f(a)(b)
}

fun <A, B, C> kFlip(f: (A, B) -> C): (B, A) -> C = { b: B, a: A ->
    f(a, b)
}

fun <A, B, C> kFlipTupled(f: (Pair<A, B>) -> C): (Pair<B, A>) -> C = { pair ->
    f(Pair(pair.second, pair.first))
}

fun <A, B, C> kFlipCurried(f: (A) -> (B) -> C): (B) -> (A) -> C = { b: B ->
    { a: A -> f(a)(b) }
}