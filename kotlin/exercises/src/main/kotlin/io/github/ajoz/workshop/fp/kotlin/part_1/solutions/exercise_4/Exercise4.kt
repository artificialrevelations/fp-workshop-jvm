@file:Suppress("PackageName")

package io.github.ajoz.workshop.fp.kotlin.part_1.solutions.exercise_4

// Part 2
fun <A, B, C> tuple(function: (A, B) -> C): (Pair<A, B>) -> C = { pair ->
    function(pair.first, pair.second)
}

// Part 3
fun <A, B, C> unTuple(function: (Pair<A, B>) -> C): (A, B) -> C = { a: A, b: B ->
    function(Pair(a, b))
}

// Part 4
internal object TupleExample {
    private val addPrefixUT: (String, String) -> String = { prefix, text ->
        prefix + text
    }

    private val addIntsT: (Pair<Int, Int>) -> Int = { pint ->
        pint.first + pint.second
    }

    @JvmStatic
    fun main(args: Array<String>) {
        val addPrefixT: (Pair<String, String>) -> String =
                tuple(addPrefixUT)

        println(addPrefixT(Pair("https://", "nozama.com")))

        val addIntsUT: (Int, Int) -> Int =
                unTuple(addIntsT)

        println(addIntsUT(42, 0))
    }
}

// Part 5
fun <A, B, C> curry(function: (A, B) -> C): (A) -> (B) -> C = { a: A ->
    { b: B ->
        function(a, b)

    }
}

fun <A, B, C> ((A, B) -> C).curried(): (A) -> (B) -> C =
        curry(this)

// Part 6
fun <A, B, C> unCurry(function: (A) -> (B) -> C): (A, B) -> C = { a: A, b: B ->
    function(a)(b)
}

// Part 7
internal object CurryExample {
    @JvmStatic
    fun main(args: Array<String>) {
        // curried
        // simple int multiplication
        val multiplyC: (Int) -> (Int) -> Int =
                { a -> { b -> a * b } }

        // use `uncurry` function to solve this
        val multiplyUC: (Int, Int) -> Int =
                unCurry(multiplyC)

        println(multiplyUC(42, 1))

        // uncurried:
        // repeat the same text specified number of times
        val replicateUC: (Int, String) -> String = { times, str ->
            val result = StringBuilder()
            for (i in 0 until times) {
                result.append(str)
            }
            result.toString()
        }

        val replicateC: (Int) -> (String) -> String =
                curry(replicateUC)

        println(replicateC(42)("JUG"))
    }
}

// Part 8
fun <A, B, C> flip(function: (A, B) -> C): (B, A) -> C = { b: B, a: A ->
    function(a, b)
}

// the same as above but in the extension form
fun <A, B, C> ((A, B) -> C).flipped(): (B, A) -> C =
        flip(this)

// Part 9
fun <A, B, C> flipTupled(function: (Pair<A, B>) -> C): (Pair<B, A>) -> C = { pair ->
    function(Pair(pair.second, pair.first))
}

// Part 10
fun <A, B, C> flipCurried(function: (A) -> (B) -> C): (B) -> (A) -> C = { b: B ->
    { a: A -> function(a)(b) }
}

// the same as above but in the extension form
fun <A, B, C> ((A) -> (B) -> C).flipped(): (B) -> (A) -> C =
        flipCurried(this)

