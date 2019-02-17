@file:Suppress("unused")

package io.github.ajoz.workshop.fp.kotlin.tools

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicReference

fun <A, B> ((A) -> B).memoized(): (A) -> B {
    val memo = ConcurrentHashMap<A, B>()
    return { a: A ->
        if (!memo.containsKey(a)) {
            memo[a] = invoke(a)
        }
        memo[a]!!
    }
}

fun <A, B, C> ((A, B) -> C).memoized(): (A, B) -> C {
    val memo = ConcurrentHashMap<Pair<A, B>, C>()
    return { a: A, b: B ->
        val key = Pair(a, b)
        if (!memo.containsKey(key)) {
            memo[key] = invoke(a, b)
        }
        memo[key]!!
    }
}

// flips the order of the arguments of a two argument function
fun <A, B, C> ((A, B) -> C).flipped() =
        { b: B, a: A ->
            this(a, b)
        }

// changes the function to a curried form
fun <A, B, C> ((A, B) -> C).curried() =
        { a: A ->
            { b: B ->
                this(a, b)
            }
        }

// applies the first argument and returns a one argument function
infix fun <A, B, C> ((A, B) -> C).applyFirst(supplier: () -> A): (B) -> C =
        { b: B ->
            this(supplier(), b)
        }

// applies the second argument and returns a one argument function
infix fun <A, B, C> ((A, B) -> C).applySecond(supplier: () -> B): (A) -> C =
        { a: A ->
            this(a, supplier())
        }

// composes two functions
infix fun <A, B, C> ((A) -> B).andThen(after: (B) -> C): (A) -> C =
        { a: A ->
            after(this(a))
        }


// composes two functions
infix fun <A, B, C> ((A) -> B).compose(before: (C) -> A): (C) -> B =
        { c: C ->
            this(before(c))
        }

// returns a function that takes a value and returns it
fun <A> identity(): (A) -> A =
        { a: A ->
            a
        }

// returns a function that takes a value and returns always the same value
fun <A, B> constant(value: B): (A) -> B =
        {
            value
        }