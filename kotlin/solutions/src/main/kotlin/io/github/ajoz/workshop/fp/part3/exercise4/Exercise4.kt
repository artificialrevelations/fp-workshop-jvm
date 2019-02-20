@file:Suppress("PackageName")

package io.github.ajoz.workshop.fp.part3.exercise4

import io.github.ajoz.workshop.fp.part3.exercise4.SealedList.Cons
import io.github.ajoz.workshop.fp.part3.exercise4.SealedList.Nil

sealed class SealedList<out A> {
    object Nil : SealedList<Nothing>() {
        override fun head() = throw NoSuchElementException("Head of empty list!")
        override fun tail() = throw NoSuchElementException("Tail of empty list!")
        override fun toString() = "Nil"
    }

    class Cons<A>(val head: A, val tail: SealedList<A>) : SealedList<A>() {
        override fun head() = head
        override fun tail() = tail
        override fun toString() = "Cons($head, $tail)"
    }

    abstract fun head(): A
    abstract fun tail(): SealedList<A>
}

fun main(args: Array<String>) {
    // A new list is created
    val sealedList = Cons(1, Cons(2, Cons(3, Nil)))

    println("sealedList = $sealedList")
}