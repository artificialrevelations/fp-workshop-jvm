@file:Suppress("PackageName")

package io.github.ajoz.workshop.fp.kotlin.part_1.solutions.exercise_4

val example2ArgFunction: (Int, Int) -> Int = { a: Int, b: Int ->
    a + b
}

fun <A, B, C> tuple(function: (A, B) -> C): (Pair<A, B>) -> C = { pair ->
    function(pair.first, pair.second)
}

fun <A, B, C> curry(function: (A, B) -> C): (A) -> (B) -> C = { a: A ->
    { b: B ->
        function(a, b)

    }
}

fun <A, B, C> ((A, B) -> C).curried(): (A) -> (B) -> C =
        curry(this)

fun <A, B, C> unTuple(function: (Pair<A, B>) -> C): (A, B) -> C = { a: A, b: B ->
    function(Pair(a, b))
}

fun <A, B, C> unCurry(function: (A) -> (B) -> C): (A, B) -> C = { a: A, b: B ->
    function(a)(b)
}

fun <A, B, C> flip(function: (A, B) -> C): (B, A) -> C = { b: B, a: A ->
    function(a, b)
}

fun <A, B, C> ((A, B) -> C).flipped(): (B, A) -> C =
        flip(this)

fun <A, B, C> flipTupled(function: (Pair<A, B>) -> C): (Pair<B, A>) -> C = { pair ->
    function(Pair(pair.second, pair.first))
}

fun <A, B, C> flipCurried(function: (A) -> (B) -> C): (B) -> (A) -> C = { b: B ->
    { a: A -> function(a)(b) }
}

fun <A, B, C> ((A) -> (B) -> C).flipped(): (B) -> (A) -> C =
        flipCurried(this)

