@file:Suppress("PackageName")

package io.github.ajoz.workshop.fp.kotlin.part_3.solutions.exercise_2

import io.github.ajoz.workshop.fp.kotlin.tools.also

sealed class SealedBoolean {
    object True : SealedBoolean() {
        override fun and(other: SealedBoolean) = other
        override fun or(other: SealedBoolean) = this
        override operator fun not() = False

        override fun ifTrue(effect: () -> Unit) = also(effect)
        override fun ifFalse(effect: () -> Unit) = this

        override fun <A> match(ifTrue: () -> A, ifFalse: () -> A) = ifTrue()

        override fun toString(): String {
            return "SealedTrue"
        }
    }

    object False : SealedBoolean() {
        override fun and(other: SealedBoolean) = this
        override fun or(other: SealedBoolean) = other
        override operator fun not() = True

        override fun ifTrue(effect: () -> Unit) = this
        override fun ifFalse(effect: () -> Unit) = also(effect)

        override fun <A> match(ifTrue: () -> A, ifFalse: () -> A) = ifFalse()

        override fun toString(): String {
            return "SealedFalse"
        }
    }

    abstract infix fun and(other: SealedBoolean): SealedBoolean
    abstract infix fun or(other: SealedBoolean): SealedBoolean
    abstract operator fun not(): SealedBoolean

    abstract fun ifTrue(effect: () -> Unit): SealedBoolean
    abstract fun ifFalse(effect: () -> Unit): SealedBoolean

    abstract fun <A> match(ifTrue: () -> A, ifFalse: () -> A): A

    companion object {
        val TRUE: SealedBoolean = True
        val FALSE: SealedBoolean = False
    }
}

fun main(args: Array<String>) {
    // simple boolean algebra:
    // and:
    println(SealedBoolean.TRUE and SealedBoolean.TRUE)
    println(SealedBoolean.TRUE and SealedBoolean.FALSE)
    println(SealedBoolean.FALSE and SealedBoolean.FALSE)
    println(SealedBoolean.FALSE and SealedBoolean.TRUE)

    // or:
    println(SealedBoolean.TRUE or SealedBoolean.FALSE)
    println(SealedBoolean.FALSE or SealedBoolean.TRUE)
    println(SealedBoolean.FALSE or SealedBoolean.FALSE)
    println(SealedBoolean.TRUE or SealedBoolean.TRUE)

    // not:
    println(SealedBoolean.TRUE.not())
    println(SealedBoolean.FALSE.not())

    // conditions as methods:
    SealedBoolean.TRUE
            .ifTrue { println("SealedTrue is SealedTrue!") }
            .ifFalse { println("SealedTrue is SealedFalse?") }

    SealedBoolean.FALSE
            .ifFalse { println("SealedFalse is SealedFalse!") }
            .ifTrue { println("SealedFalse is SealedTrue?") }

    // conditions as expressions:
    val trueMessage = SealedBoolean.TRUE
            .match(
                    { "Matching SealedTrue to SealedTrue!" },
                    { "Matching SealedTrue to SealedFalse?" }
            )
    println(trueMessage)

    val falseMessage = SealedBoolean.FALSE
            .match(
                    { "Matching SealedFalse to SealedTrue?" },
                    { "Matching SealedFalse to SealedFalse!" }
            )
    println(falseMessage)

    val bool = SealedBoolean.FALSE
    when (bool) {
        SealedBoolean.FALSE -> println("Switch FALSE")
        SealedBoolean.TRUE -> println("Switch TRUE")
    }
}
