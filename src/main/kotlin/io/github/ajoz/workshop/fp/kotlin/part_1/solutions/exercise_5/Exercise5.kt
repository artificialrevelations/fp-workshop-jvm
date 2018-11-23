@file:Suppress("PackageName")

package io.github.ajoz.workshop.fp.kotlin.part_1.solutions.exercise_5

fun <A, B> composeConsumer(function: (A) -> B, consumer: (B) -> Unit): (A) -> Unit = { a: A ->
    consumer(function(a))
}

fun <A, B> composeSupplier(function: (A) -> B, supplier: () -> A): () -> B = {
    function(supplier())
}

fun <A, B, C> applyCurriedFirst(function: (A) -> (B) -> C, supplier: () -> A): (B) -> C = { b: B ->
    function(supplier())(b)
}

fun <A, B, C> applyCurriedSecond(function: (A) -> (B) -> C, supplier: () -> B): (A) -> C = { a: A ->
    function(a)(supplier())
}