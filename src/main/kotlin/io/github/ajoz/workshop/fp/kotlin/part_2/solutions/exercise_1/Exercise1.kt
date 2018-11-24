@file:Suppress("PackageName", "UNUSED_PARAMETER")

package io.github.ajoz.workshop.fp.kotlin.part_2.solutions.exercise_1

fun sum(list: List<Int>): Int {
    var sum = 0
    for (value in list) {
        sum += value
    }
    return sum
}

fun product(list: List<Int>): Int {
    var sum = 1
    for (value in list) {
        sum *= value
    }
    return sum
}


// for the abstracted function we will use a name `foo`
// hint:
// - what arguments do we need to pass to `foo`?
// - `sum` is using + operator, `product` is using * operator
//   how can we abstract these two operators?
// - do not use any conditional statements for this exercise
fun foo(list: List<Int>, initial: Int, operator: (Int, Int) -> Int): Int {
    var accumulator = initial
    for (value in list) {
        accumulator = operator(value, accumulator)
    }
    return accumulator
}

fun sum2(list: List<Int>): Int {
    return foo(list, 0) { a, b ->
        a + b
    }
}

fun product2(list: List<Int>): Int {
    return foo(list, 1) { a, b ->
        a * b
    }
}
