@file:Suppress("PackageName")

package io.github.ajoz.workshop.fp.kotlin.part_4.solutions.exercise_3

import java.util.concurrent.ConcurrentHashMap

// Part 1:

internal fun <A, B> ((A) -> B).memoized(): (A) -> B {
    val memo = ConcurrentHashMap<A, B>()
    return { a: A ->
        if (!memo.containsKey(a)) {
            memo[a] = invoke(a)
        }
        memo[a]!!
    }
}

// Part 2:
internal fun <A, B, C> ((A, B) -> C).memoized(): (A, B) -> C {
    val memo = ConcurrentHashMap<Pair<A, B>, C>()
    return { a: A, b: B ->
        val key = Pair(a, b)
        if (!memo.containsKey(key)) {
            memo[key] = invoke(a, b)
        }
        memo[key]!!
    }
}

fun main(args: Array<String>) {
    // Part 1:
    val fun1: (String) -> Int = { arg: String ->
        println(String.format("Argument passed: %s", arg))
        arg.length
    }.memoized()

    (0..9).forEach { println(fun1("#$it JUG Lodz")) }
    (0..9).forEach { println(fun1("#$it Mobilization Conference")) }

    // Part 2:
    val fun2: (String, String) -> Int = { arg1: String, arg2: String ->
        println(String.format("Arguments passed: %s, %s", arg1, arg2))
        (arg1 + arg2).length
    }.memoized()

    (0..9).forEach { println(fun2("#$it JUG", "#$it Lodz")) }
    (0..9).forEach { println(fun2("#$it Mobilization", "#$it Conference")) }
}
