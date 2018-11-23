@file:Suppress("PackageName")

package io.github.ajoz.workshop.fp.kotlin.part_1.solutions.exercise_2

internal interface KFunction1<A, B> {
    operator fun invoke(argument: A): B
}

fun main(args: Array<String>) {
    val function = object : KFunction1<String, String> {
        override fun invoke(argument: String) = "${argument}foo"
    }

    printFunction("bar", function)
    printFunction("baz", function)
    printFunction("qux", function)

    // Let's use the default function type in Kotlin and with it all the
    // nice syntactic sugar we have available
    val xPlusOne: (Int) -> Int = { x -> x + 1 }
    val xPlusTwoSquared: (Int) -> Int = { x -> (x + 2) * (x + 2) }

    printKotlinFunction(1, xPlusOne)
    printKotlinFunction(3, xPlusTwoSquared)

    // instead of writing: val minusXPlusTen: (Int) -> Int = { x -> (-x) + 10 }
    // we can use some sugar here:
    printKotlinFunction(5) { x ->
        (-x) + 10
    }

    // for easier reading we will drop f and g as a name
    // and use something more descriptive
    val stringToInt: (String) -> Int = { Integer.valueOf(it) }
    val intToBool: (Int) -> Boolean = { it == 42 }

    val stringToBool: (String) -> Boolean = { string ->
        val result = stringToInt(string)
        intToBool(result)
    }

    printKotlinFunction("41", stringToBool)
    printKotlinFunction("42", stringToBool)
}

private fun <A, B> printFunction(argument: A,
                                 function: KFunction1<A, B>) {
    println(String.format("kx: A = %s ky: B = %s", argument, function(argument)))
}

private fun <A, B> printKotlinFunction(argument: A,
                                       function: (A) -> B) {
    println(String.format("> kx: A = %s ky: B = %s", argument, function(argument)))
}