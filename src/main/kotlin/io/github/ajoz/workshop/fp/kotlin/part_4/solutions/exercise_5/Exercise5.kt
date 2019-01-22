@file:Suppress("PackageName")

package io.github.ajoz.workshop.fp.kotlin.part_4.solutions.exercise_5

import io.github.ajoz.workshop.fp.kotlin.tools.control.Try
import java.util.Arrays
import java.util.LinkedList
import java.util.NoSuchElementException

internal interface Flow<A> {
    fun next(): Try<A>
}

internal fun <A, B> Flow<A>.map(mapper: (A) -> B): Flow<B> {
    return MapFlow(this, mapper)
}

internal fun <A> Flow<A>.toList(): List<A> {
    val list = LinkedList<A>()
    while (true) {
        val element = next()
        if (element.isFailure)
            break

        list.add(element.get())
    }
    return list
}

internal object Flows {
    fun <A> of(list: List<A>): Flow<A> {
        return ListFlow(list)
    }
}

internal class ListFlow<A>(private val list: List<A>) : Flow<A> {
    private var current: Int = 0

    override fun next(): Try<A> {
        if (list.isEmpty())
            return Try.Failure(NoSuchElementException("No elements in this Flow!"))

        if (current >= list.size)
            return Try.Failure(NoSuchElementException("No more elements in this Flow!"))

        val next = Try.Success(list[current])
        current++
        return next
    }
}

internal class MapFlow<A, B>(
        val upstream: Flow<A>,
        val mapper: (A) -> B
) : Flow<B> {
    override fun next() = upstream.next().map(mapper)
}

fun main(args: Array<String>) {
    val l1 = Flows.of(Arrays.asList(1, 2, 3)).toList()
    println("Flow.of([1, 2, 3]) = $l1")

    val l2 = Flows.of(Arrays.asList("JUG", "Lodz")).map { it.length }.toList()

    println("Flow.of([\"JUG\", \"Lodz\"]).map(String::length) = $l2")
}
