package io.github.ajoz.workshop.fp.solutions.exercise_1

// Although Kotlin has a Function type defined, for the exercise purpose
// we will create a class that is expressing it


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
    val function: KFunction1 = object : KFunction1() {
        override fun invoke(argument: Int): Int =
                argument + 1
    }

    printFunction(function, 1)
    printFunction(function, 2)
    printFunction(function, 3)
    printFunction(function, 4)
}

fun printFunction(function: KFunction1,
                  argument: Int) {
    println("kx = $argument ky = ${function.invoke(argument)}")
}
