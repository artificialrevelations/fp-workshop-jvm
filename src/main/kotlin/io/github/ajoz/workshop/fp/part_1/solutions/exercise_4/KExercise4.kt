@file:Suppress("PackageName")

package io.github.ajoz.workshop.fp.part_1.solutions.exercise_4

val example2ArgFunction: (Int, Int) -> Int = { a: Int, b: Int ->
    a + b
}

fun <A, B, C> kConvertToFunction1WithPair(f: (A, B) -> C): (Pair<A, B>) -> C = { pair ->
    f(pair.first, pair.second)
}

fun <A, B, C> kConvertToFunction1WithFunction(f: (A, B) -> C): (A) -> (B) -> C = { a: A ->
    { b: B ->
        f(a, b)

    }
}

fun <A, B, C> kConvertToFunction2FromPair(f: (Pair<A, B>) -> C): (A, B) -> C = { a: A, b: B ->
    f(Pair(a, b))
}

fun <A, B, C> convertToFunction2FromFunction(f: (A) -> (B) -> C): (A, B) -> C = { a: A, b: B ->
    f(a)(b)
}