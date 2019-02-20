@file:Suppress("PackageName", "MoveLambdaOutsideParentheses")

package io.github.ajoz.workshop.fp.part4.exercise6

import io.github.ajoz.workshop.fp.part4.exercise6.Flows.cycle
import io.github.ajoz.workshop.fp.part4.exercise6.Flows.generate
import io.github.ajoz.workshop.fp.tools.control.Try
import java.util.LinkedList
import java.util.NoSuchElementException

import java.util.Arrays.asList

internal interface Flow<A> {
    fun next(): Try<A>
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

// Part 1
internal class ZippingFlow<A, B, C>(private val left: Flow<A>,
                                    private val right: Flow<B>,
                                    private val zipper: (A, B) -> C) : Flow<C> {

    override fun next(): Try<C> {
        val nextLeft = left.next()
        if (nextLeft.isFailure)
            return Try.Failure(NoSuchElementException("Left flow is out of elements to zip!"))

        val nextRight = right.next()
        return if (nextRight.isFailure)
            Try.Failure(NoSuchElementException("Right flow is out of elements to zip!"))
        else nextLeft.flatMap { a ->
            nextRight.flatMap { b ->
                Try.Success(zipper(a, b))
            }
        }
    }
}

internal fun <A, B, C> Flow<A>.zip(other: Flow<B>,
                                   zipper: (A, B) -> C): Flow<C> =
        ZippingFlow(this, other, zipper)

// Part 2:
internal class CycleListFlow<A>(private val list: List<A>) : Flow<A> {
    private var current: Int = 0

    override fun next(): Try<A> {
        if (list.isEmpty())
            return Try.Failure(NoSuchElementException("No elements in this Flow!"))

        if (current >= list.size)
            current = 0

        val next = Try.Success(list[current])
        current++
        return next
    }
}

// Part 3:
internal class GenerateFlow<A>(private var seed: A,
                               private val generator: (A) -> A) : Flow<A> {

    override fun next(): Try<A> {
        val next = Try.Success(seed)
        seed = generator(seed)
        return next
    }
}

internal object Flows {
    fun <A> cycle(list: List<A>): Flow<A> =
            CycleListFlow(list)

    fun <A> generate(seed: A, generator: (A) -> A): Flow<A> =
            GenerateFlow(seed, generator)
}

internal class TakeFlow<A>(private val upstream: Flow<A>,
                           private val amount: Int) : Flow<A> {

    private var taken: Int = 0

    override fun next(): Try<A> {
        // take an element from the upstream Flow
        val next = upstream.next()
        // if the element does not exist then just propagate the failure
        if (next.isFailure)
            return next

        // if already taken enough elements then just propagate the failure
        if (taken >= amount) {
            return Try.Failure(NoSuchElementException("Reached the take threshold: $amount"))
        }

        // increment the currently taken amount
        taken++
        // return the upstream Flow item
        return next
    }
}

internal fun <A> Flow<A>.take(threshold: Int): Flow<A> =
        TakeFlow(this, threshold)


fun main(args: Array<String>) {
    val fizzFlow = cycle(asList("", "", "Fizz"))
    val buzzFlow = cycle(asList("", "", "", "", "Buzz"))
    val numbers = generate(1) { a -> a + 1 }

    val fizzBuzz = fizzFlow
            .zip(buzzFlow) { a, b ->
                a + b
            }
            .zip(numbers) { string, integer ->
                if (string.isEmpty())
                    "$integer"
                else
                    string
            }
            .take(200)
            .toList()

    println("fizzBuzz = $fizzBuzz")
}
