@file:Suppress("PackageName")

package io.github.ajoz.workshop.fp.kotlin.part_2.solutions.exercise_5

import java.util.ArrayList

// Part 1:
fun mapInts(list: List<Int>,
            mapper: (Int) -> Int): List<Int> {
    val mapped = ArrayList<Int>(list.size) //root of all evil!
    for (value in list) {
        mapped.add(mapper(value))
    }
    return mapped
}

// Part 2:
fun <A, B> map(list: List<A>,
               mapper: (A) -> B): List<B> {
    val mapped = ArrayList<B>(list.size) //root of all evil!
    for (value in list) {
        mapped.add(mapper(value))
    }
    return mapped
}

// Part 3:
fun addOne(list: List<Int>): List<Int> {
    return map(list) { integer -> integer + 1 }
}

// Part 4:
fun lengths(strings: List<String>): List<Int> {
    return map(strings) { it.length }
}