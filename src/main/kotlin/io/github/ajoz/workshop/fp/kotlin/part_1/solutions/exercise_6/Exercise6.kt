@file:Suppress("PackageName")

package io.github.ajoz.workshop.fp.kotlin.part_1.solutions.exercise_6

class Customer(val name: String)

class Order(val title: String, val date: Long?)

class Database {
    // for now let's assume that the DB will always return an Order for a
    // given Customer
    fun findOrder(customer: Customer): Order {
        return Order("JUG Łódź -- visit our FB, twitter and meetup!", 42L)
    }
}

object KExercise6 {
    // Please do not change this function!
    private val getOrderForCustomer: (Customer, Database) -> Order =
            { customer, database ->
                database.findOrder(customer)
            }

    // Please do not change this function!
    private val getProductionDatabase = { Database() }

    // Please do not change this function!
    private val getOrderTitle: (Order) -> String = { order ->
        order.title
    }

    // Please do not change this function!
    private val getHash: (String) -> Long = {
        42L
    }

    fun customerToHash(): (Customer) -> Long =
            getOrderForCustomer.flip().curry() apF getProductionDatabase andThen getOrderTitle andThen getHash

}

fun <A, B, C> kApplyFirst(f: (A) -> (B) -> C, s: () -> A): (B) -> C = { b: B ->
    f(s())(b)
}

fun <A, B, C> kFlip(f: (A, B) -> C): (B, A) -> C = { b: B, a: A ->
    f(a, b)
}

fun <A, B, C> kCurry(f: (A, B) -> C): (A) -> (B) -> C = { a: A ->
    { b: B ->
        f(a, b)

    }
}

fun <A, B, C> ((A, B) -> C).flip() = kFlip(this)

fun <A, B, C> ((A, B) -> C).curry() = kCurry(this)

infix fun <A, B, C> ((A) -> (B) -> C).apF(s: () -> A): (B) -> C =
        kApplyFirst(this, s)

infix fun <A, B, C> ((A) -> B).andThen(after: (B) -> C): (A) -> C = { a: A ->
    after(this(a))
}