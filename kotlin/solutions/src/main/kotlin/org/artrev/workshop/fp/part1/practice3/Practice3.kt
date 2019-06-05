@file:Suppress("unused", "UNUSED_PARAMETER")

package org.artrev.workshop.fp.part1.practice3

import org.artrev.workshop.fp.part1.practice1.not
import org.artrev.workshop.fp.tools.and
import org.artrev.workshop.fp.tools.curry
import org.artrev.workshop.fp.tools.or
import org.artrev.workshop.fp.tools.predicates.isLargerThen
import org.artrev.workshop.fp.tools.predicates.isLowerThen

internal object Lists {
    // Part 1:
    fun <A> select(predicate: (A) -> Boolean, elements: List<A>): List<A> {
        val selected = mutableListOf<A>()
        for (element in elements) {
            if (predicate(element))
                selected.add(element)
        }
        return selected
    }

    // Part 2:
    fun <A> reject(predicate: (A) -> Boolean, elements: List<A>) =
            select(predicate.not(), elements)
}

// Part 3:
fun allAbove42(values: List<Int>): List<Int> =
        Lists.select(isLargerThen(42), values)

// Part 4:
var allBelow42: (List<Int>) -> List<Int> =
        curry<(Int) -> Boolean, List<Int>, List<Int>>(Lists::select)(isLowerThen(42))

// Part 5:
private val hasFirstCapitalLetter: (String) -> Boolean =
        { it.first().isUpperCase() }

private val hasLastCapitalLetter: (String) -> Boolean =
        { it.last().isUpperCase() }

private fun hasLengthLargerThen(length: Int): (String) -> Boolean =
        { it.length > length }

private fun hasLengthSmallerThen(length: Int): (String) -> Boolean =
        { it.length < length }

private fun contains(text: String): (String) -> Boolean =
        { it.contains(text) }

private val hasLengthInRange =
        hasLengthLargerThen(3).and(hasLengthSmallerThen(10))

private val hasCorrectFirstOrLastLetter =
        hasFirstCapitalLetter.or(hasLastCapitalLetter)

fun getStrings(data: List<String>) =
        Lists.select(
                hasLengthInRange.and(hasCorrectFirstOrLastLetter).and(contains("JUG")),
                data
        )
