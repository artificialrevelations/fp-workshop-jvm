@file:Suppress("PackageName")

package io.github.ajoz.workshop.fp.kotlin.part_2.solutions.exercise_3

import io.github.ajoz.workshop.fp.java.tools.collections.Lists
import io.github.ajoz.workshop.fp.kotlin.tools.flipped

// Part 1:
fun <A, B> foldLeft(list: List<A>,
                    initial: B,
                    operator: (B, A) -> B): B {
    var accumulator = initial
    for (element in list) {
        accumulator = operator(accumulator, element)
    }

    return accumulator
}

// Part 2:
fun <A, B> foldRight(list: List<A>,
                     initial: B,
                     operator: (A, B) -> B): B {
    var accumulator = initial
    for (i in list.size downTo 1) {
        accumulator = operator(list[i - 1], accumulator)
    }
    return accumulator
}

// Part 3:
fun <A, B> foldRight2(list: List<A>,
                      initial: B,
                      operator: (A, B) -> B): B {
    return foldLeft(
            Lists.reverse(list),
            initial,
            operator.flipped()
    )
}