@file:Suppress("PackageName")

package io.github.ajoz.workshop.fp.part_1.solutions.exercise_3

fun kComposeIntFuns(first: (Int) -> Int, second: (Int) -> Int): (Int) -> Int =
        { value ->
            second(first(value))
        }

fun <A, B, C> kCompose(f: (A) -> B, g: (B) -> C): (A) -> C =
        { a: A ->
            g(f(a))
        }

fun <A> kComposeAll(vararg functions: (A) -> A): (A) -> A
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
        result = kCompose(result, function)
    }
    return result
}

infix fun <A, B, C> ((A) -> B).andThen(after: (B) -> C): (A) -> C = { a: A ->
    after(this(a))
}

infix fun <A, B, C> ((A) -> B).compose(before: (C) -> A): (C) -> B = { c: C ->
    this(before(c))
}