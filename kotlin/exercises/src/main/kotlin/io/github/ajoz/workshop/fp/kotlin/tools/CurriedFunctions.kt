package io.github.ajoz.workshop.fp.kotlin.tools


// applies the first argument of the curried function from a given supplier
fun <A, B, C> applyFirst(function: (A) -> (B) -> C, supplier: () -> A): (B) -> C =
        { b: B ->
            function(supplier())(b)
        }

// applyFirst version that is an infix extension function
infix fun <A, B, C> ((A) -> (B) -> C).apF(supplier: () -> A): (B) -> C =
        applyFirst(this, supplier)

// applies the second argument of the curried function from a given supplier
fun <A, B, C> applySecond(function: (A) -> (B) -> C, supplier: () -> B): (A) -> C =
        { a: A ->
            function(a)(supplier())
        }

// applySecond version that is an infix extension function
infix fun <A, B, C> ((A) -> (B) -> C).apS(supplier: () -> B): (A) -> C =
        applySecond(this, supplier)