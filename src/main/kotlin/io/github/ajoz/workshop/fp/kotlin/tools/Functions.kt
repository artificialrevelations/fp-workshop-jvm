package io.github.ajoz.workshop.fp.kotlin.tools

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