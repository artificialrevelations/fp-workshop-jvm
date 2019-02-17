@file:Suppress("PackageName")

package io.github.ajoz.workshop.fp.kotlin.part_2.solutions.exercise_6

import java.util.*

object Exercise6 {

    // Part 1:
    fun <A, B> foldLeftCurried(operator: (B) -> (A) -> B): (B) -> (List<A>) -> B {
        return { initial: B ->
            { list: List<A> ->
                var accumulator = initial
                for (element in list) {
                    accumulator = operator(accumulator)(element)
                }

                accumulator
            }
        }
    }

    // Part 2:
    var sum = foldLeftCurried { a: Int -> { b: Int -> a + b } }(0)

    var product = foldLeftCurried { a: Int -> { b: Int -> a + b } }(1)

    // Part 3:
    fun <A, B> mapCurried(mapper: (A) -> B): (List<A>) -> List<B> {
        return { list: List<A> ->
            val mapped = ArrayList<B>(list.size)
            for (a in list) {
                mapped.add(mapper(a))
            }
            mapped
        }
    }

    // Part 4:
    var lengths = mapCurried { str: String -> str.length }
}