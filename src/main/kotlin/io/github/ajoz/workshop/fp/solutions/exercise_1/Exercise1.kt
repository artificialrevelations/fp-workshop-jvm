@file:Suppress("PackageName", "PARAMETER_NAME_CHANGED_ON_OVERRIDE")

package io.github.ajoz.workshop.fp.solutions.exercise_1

// We can express a one argument function in the form of an abstract class:
abstract class KFunction1 {
    // what will happen if we will add operator keyword?
    abstract fun invoke(argument: Int): Int
}

// We can express a one argument function in the form of an interface:
/*
interface KFunction1 {
    fun invoke(argument: Int): Int
}
*/

fun main(args: Array<String>) {
    val xPlusOne: KFunction1 = object : KFunction1() {
        // although the argument in our Kotlin version is called
        // "argument" we change its name to "x" so its more familiar
        // beware that this conflicts with Kotlin named arguments feature
        override fun invoke(x: Int): Int = x + 1
    }

    val xPlusTwoSquared = object : KFunction1() {
        override fun invoke(x: Int) = (x + 2) * (x + 2)
    }

    val minusXPlusTen = object : KFunction1() {
        override fun invoke(x: Int) = -x + 10
    }

    printFunction(xPlusOne, 1)
    printFunction(xPlusOne, 2)

    printFunction(xPlusTwoSquared, 3)
    printFunction(xPlusTwoSquared, 4)

    printFunction(minusXPlusTen, 5)
    printFunction(minusXPlusTen, 6)

//    Check how does Kotlin behave with changed names of arguments:
//    println("test of named argument: ${xPlusOne.invoke(argument = 42)}")
//    println("test of named argument: ${xPlusOne.invoke(x = 42)}")
}

fun printFunction(function: KFunction1,
                  argument: Int) {
    println("kx = $argument ky = ${function.invoke(argument)}")
}
