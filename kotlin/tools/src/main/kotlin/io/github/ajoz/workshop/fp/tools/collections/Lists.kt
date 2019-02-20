package io.github.ajoz.workshop.fp.tools.collections

import java.util.ArrayList
import java.util.LinkedList
import java.util.NoSuchElementException

import java.util.Collections.emptyList

// returns a list with elements in the reverse order
fun <A> reverse(list: List<A>): List<A> {
    val revered = ArrayList<A>(list.size)
    for (i in list.indices.reversed()) {
        revered.add(list[i])
    }
    return revered
}

// returns a head of a list (first element)
// if there is no element then throws a NoSuchElementException
fun <A> head(list: List<A>): A {
    if (list.isEmpty())
        throw NoSuchElementException("head of empty list")
    return list[0]
}

// returns a tail of a list (all elements except the head)
// if there is no element then throws a NoSuchElementException
// if there is only one element then an empty list is returned
fun <A> tail(list: List<A>): List<A> {
    return when {
        list.isEmpty() -> throw UnsupportedOperationException("tail of empty list")
        list.size == 1 -> emptyList()
        else -> list.subList(1, list.size)
    }
}

// Prepends the given item at the beginning of the list.
fun <A> prepend(item: A, list: List<A>): List<A> {
    val newList = LinkedList<A>() //to construct a new list
    newList.add(item)
    newList.addAll(list)
    return newList
}

// Appends the given item at the end of the given list.
fun <A> append(list: List<A>, item: A): List<A> {
    val newList = LinkedList(list)
    newList.add(item)
    return newList
}

// Folds the list from the left
fun <A, B> foldLeft(list: List<A>,
                    initial: B,
                    operator: (B, A) -> B): B {
    var accumulator = initial
    for (element in list) {
        accumulator = operator(accumulator, element)
    }

    return accumulator
}

// Fold the list from the right
fun <A, B> foldRight(list: List<A>,
                     initial: B,
                     operator: (A, B) -> B): B {
    var accumulator = initial
    for (i in list.size downTo 1) {
        accumulator = operator(list[i - 1], accumulator)
    }
    return accumulator
}

fun sum(list: List<Int>): Int {
    return foldLeft(list, 0) { a, b -> a + b }
}

fun product(list: List<Int>): Int {
    return foldLeft(list, 1) { a, b -> a * b }
}