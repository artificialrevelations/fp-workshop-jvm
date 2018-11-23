@file:Suppress("PackageName")

package io.github.ajoz.workshop.fp.kotlin.part_1.solutions.exercise_3

fun composeIntFuns(first: (Int) -> Int, second: (Int) -> Int): (Int) -> Int =
        { value ->
            second(first(value))
        }

fun <A, B, C> kompose(f: (A) -> B, g: (B) -> C): (A) -> C =
        { a: A ->
            g(f(a))
        }

fun <A> composeAll(vararg functions: (A) -> A): (A) -> A
// as function application:
//        = { a: A ->
//            var result = a
//            for (function in functions) {
//                result = function(result)
//            }
//            result
//        }
{
    var result: (A) -> A = { it }
    for (function in functions) {
        result = kompose(result, function)
    }
    return result
}

infix fun <A, B, C> ((A) -> B).andThen(after: (B) -> C): (A) -> C = { a: A ->
    after(this(a))
}

infix fun <A, B, C> ((A) -> B).compose(before: (C) -> A): (C) -> B = { c: C ->
    this(before(c))
}