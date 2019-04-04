@file:Suppress("PackageName")

package org.artrev.workshop.fp.part2.exercise3

import io.github.ajoz.workshop.fp.tools.collections.reverse
import io.github.ajoz.workshop.fp.tools.flipped

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
            reverse(list),
            initial,
            operator.flipped()
    )
}