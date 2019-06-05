@file:Suppress("PackageName")

package org.artrev.workshop.fp.part2.exercise4

import org.artrev.workshop.fp.tools.collections.*
import kotlin.Pair

fun maximum(list: List<Int>): Int? {
    if (list.isEmpty())
        throw IllegalArgumentException("list is empty")

    return foldLeft(list, Integer.MIN_VALUE) { a, b ->
        Math.max(a, b)
    }
}

fun minimum(list: List<Int>): Int? {
    if (list.isEmpty())
        throw IllegalArgumentException("list is empty")

    return foldLeft(list, Integer.MAX_VALUE) { a, b ->
        Math.min(a, b)
    }
}

fun <A> count(list: List<A>): Int? {
    return foldLeft(list, 0) { count, _ ->
        count + 1
    }
}

fun <A> last(list: List<A>): A {
    return foldLeft(list, head(list)) { _, y -> y }
}

fun <A> reverse(list: List<A>): List<A> {
    return foldLeft(list, emptyList()) { reversedList, item ->
        prepend(item, reversedList)
    }
}

fun average(list: List<Int>): Int {
    return if (list.isEmpty()) 0 else sum(list) / list.size
}

fun <A> contains(list: List<A>,
                 searched: A): Boolean? {
    return foldLeft(list, false) { contains, item ->
        contains || item == searched
    }
}

fun <A> join(list: List<A>,
             separator: String): String {
    return if (list.isEmpty())
        ""
    else
        foldLeft(tail(list), head(list).toString()) { joined, item ->
            joined + separator + item
        }
}

fun <A> penultimate(list: List<A>): A {
    if (list.isEmpty())
        throw IllegalArgumentException("list is empty")

    if (list.size == 1)
        throw IllegalArgumentException("list has only one element")

    return foldLeft(
            list,
            Pair(
                    head(list),
                    head(tail(list))
            )) { tuple, a ->
        Pair(tuple.second, a)
    }.first
}
