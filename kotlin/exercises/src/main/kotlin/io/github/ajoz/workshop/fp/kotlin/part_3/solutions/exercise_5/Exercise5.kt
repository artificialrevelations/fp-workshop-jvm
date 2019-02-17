@file:Suppress("PackageName", "UNUSED_PARAMETER", "unused")

package io.github.ajoz.workshop.fp.kotlin.part_3.solutions.exercise_5

fun div1(a: Int, b: Int): Int = a / b

class DivideByZero : Exception()

@Throws(DivideByZero::class)
fun div2(a: Int, b: Int): Int = when (b) {
    0 -> throw DivideByZero()
    else -> a / b
}

fun div3(a: Int, b: Int): Int? = when (b) {
    0 -> null
    else -> a / b
}

class Result(val value: Int?, val exists: Boolean)

fun div4(a: Int, b: Int): Result = when (b) {
    0 -> Result(null, false)
    else -> Result(a / b, true)
}

sealed class Maybe<out A> {
    data class Some<A>(val value: A) : Maybe<A>()
    object None : Maybe<Nothing>()
}

fun safeDiv(a: Int, b: Int): Maybe<Int> = when (b) {
    0 -> Maybe.None
    else -> Maybe.Some(a / b)
}

fun main(args: Array<String>) {
    // Part 1:
    // println(div1(42, 0))

    // Part 2:
    try {
        val res2 = div2(42, 0)
        println("Div2 result: $res2")
    } catch (divideByZero: DivideByZero) {
        println("Error handling after div2 failed!")
    }

    // Part 3:
    val res3 = div3(42, 0)
    if (null != res3) {
        println("Div3 result: $res3")
    } else {
        println("Error handling after div3 failed!")
    }

    // Part 4:
    val res4 = div4(42, 0)
    if (res4.exists) {
        println("Div4 result: $res4")
    } else {
        println("Error handling after div4 failed!")
    }

    // Part 5:
    val res5 = safeDiv(42, 0)
    when (res5) {
        is Maybe.Some -> println("SafeDiv result: ${res5.value}")
        is Maybe.None -> println("Error handling after safeDiv failed!")
    }
}