@file:Suppress("PackageName")

package io.github.ajoz.workshop.fp.kotlin.part_3.solutions.exercise_1

import io.github.ajoz.workshop.fp.kotlin.tools.also

enum class EnumBoolean {
    TRUE {
        override fun and(other: EnumBoolean) = other
        override fun or(other: EnumBoolean) = this
        override operator fun not() = FALSE

        // I've added a override to also to be able to pass a () -> Unit
        override fun ifTrue(effect: () -> Unit) = also(effect)

        override fun ifFalse(effect: () -> Unit) = this
        override fun <A> match(ifTrue: () -> A, ifFalse: () -> A): A = ifTrue()
    },
    FALSE {
        override fun and(other: EnumBoolean) = this
        override fun or(other: EnumBoolean) = other
        override operator fun not() = TRUE

        override fun ifTrue(effect: () -> Unit) = this
        override fun ifFalse(effect: () -> Unit) = also(effect)

        override fun <A> match(ifTrue: () -> A, ifFalse: () -> A) = ifFalse()
    };

    abstract infix fun and(other: EnumBoolean): EnumBoolean
    abstract infix fun or(other: EnumBoolean): EnumBoolean
    abstract operator fun not(): EnumBoolean

    abstract fun ifTrue(effect: () -> Unit): EnumBoolean
    abstract fun ifFalse(effect: () -> Unit): EnumBoolean

    abstract fun <A> match(ifTrue: () -> A, ifFalse: () -> A): A
}

fun main(args: Array<String>) {
    // simple boolean algebra:
    // and:
    println(EnumBoolean.TRUE and EnumBoolean.TRUE)
    println(EnumBoolean.TRUE and EnumBoolean.FALSE)
    println(EnumBoolean.FALSE and EnumBoolean.FALSE)
    println(EnumBoolean.FALSE and EnumBoolean.TRUE)

    // or:
    println(EnumBoolean.TRUE or EnumBoolean.FALSE)
    println(EnumBoolean.FALSE or EnumBoolean.TRUE)
    println(EnumBoolean.FALSE or EnumBoolean.FALSE)
    println(EnumBoolean.TRUE or EnumBoolean.TRUE)

    // not:
    println(EnumBoolean.TRUE.not())
    println(EnumBoolean.FALSE.not())

    // conditions as methods:
    EnumBoolean.TRUE
            .ifTrue { println("EnumTrue is EnumTrue!") }
            .ifFalse { println("EnumTrue is EnumFalse?") }

    EnumBoolean.FALSE
            .ifFalse { println("EnumFalse is EnumFalse!") }
            .ifTrue { println("EnumFalse is EnumTrue?") }

    // conditions as expressions:
    val trueMessage = EnumBoolean.TRUE
            .match(
                    { "Matching EnumTrue to EnumTrue!" },
                    { "Matching EnumTrue to EnumFalse?" }
            )
    println(trueMessage)

    val falseMessage = EnumBoolean.FALSE
            .match(
                    { "Matching EnumFalse to EnumTrue?" },
                    { "Matching EnumFalse to EnumFalse!" }
            )
    println(falseMessage)

    // using when:
    val bool = EnumBoolean.TRUE
    when (bool) {
        EnumBoolean.FALSE -> println("When FALSE")
        EnumBoolean.TRUE -> println("When TRUE")
    }
}

