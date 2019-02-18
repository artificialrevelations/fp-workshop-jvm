@file:Suppress("PackageName")

package io.github.ajoz.workshop.fp.part2.exercise2

// Part 1:
fun foo(list: List<Int>,
        initial: Int,
        operator: (Int, Int) -> Int): Int {
    var accumulator = initial
    for (element in list) {
        accumulator = operator(accumulator, element)
    }

    return accumulator
}

// Part 2:
fun <A, B> bar(list: List<A>,
               initial: B,
               operator: (B, A) -> B): B {
    // look at foo implementation and this implementation
    // isn't type inference a great thing :-)
    var accumulator = initial
    for (element in list) {
        accumulator = operator(accumulator, element)
    }

    return accumulator
}

// Part 3:
fun sum(list: List<Int>): Int {
    return bar(list, 0) { b, a ->
        b + a
    }
}

fun product(list: List<Int>): Int {
    return bar(list, 1) { b, a ->
        b * a
    }
}
