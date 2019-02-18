@file:Suppress("PackageName", "FunctionName")

package io.github.ajoz.workshop.fp.part1.exercise3

// Part 1
fun composeIntFuns(first: (Int) -> Int, second: (Int) -> Int): (Int) -> Int =
        { value ->
            second(first(value))
        }

// Part 2
fun <A, B, C> kompose(f: (A) -> B, g: (B) -> C): (A) -> C =
        { a: A ->
            g(f(a))
        }

// Part 3
fun <A> composeAll_1(vararg functions: (A) -> A): (A) -> A =
        { a: A ->
            var result = a
            for (function in functions) {
                result = function(result)
            }
            result
        }

// Part 4
fun <A> composeAll_2(vararg functions: (A) -> A): (A) -> A {
    var result: (A) -> A = { it }
    for (function in functions) {
        result = kompose(result, function)
    }
    return result
}

// Part 5
infix fun <A, B, C> ((A) -> B).andThen(after: (B) -> C): (A) -> C =
        { a: A ->
            after(this(a))
        }

// Part 6
infix fun <A, B, C> ((A) -> B).compose(before: (C) -> A): (C) -> B =
        { c: C ->
            this(before(c))
        }

// Part 7
data class Product(val id: Id, val description: Description)

data class Id(val value: String)
data class Description(val name: String)

val getProductId: (Product) -> Id = { it.id }
val getPurchaseUri: (Id) -> String = { "nozama.com/shop/purchase?=${it.value}" }
val getSecureUrl: (String) -> String = { "https://$it" }

val getSecureProductPurchaseUrl =
        getProductId andThen getPurchaseUri andThen getSecureUrl

fun main(args: Array<String>) {
    val products =
            listOf(
                    Product(
                            Id("JUGLDZ42JAVA"),
                            Description("Workshop: FP in Java")
                    ),
                    Product(
                            Id("JUGLDZ42KOTLIN"),
                            Description("Workshop: FP in Kotlin")
                    )
            )

    products.map(getSecureProductPurchaseUrl).onEach(::println)
}