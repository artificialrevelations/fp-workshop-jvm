@file:Suppress("PackageName", "unused")

package io.github.ajoz.workshop.fp.kotlin.part_1.solutions.exercise_6

import io.github.ajoz.workshop.fp.kotlin.tools.andThen
import io.github.ajoz.workshop.fp.kotlin.tools.applySecond

// other fields like: surname, address, etc.
data class Customer(val name: String)

// other fields like: amount, currency, tax, etc.
data class Order(val title: Title, val date: Timestamp)

data class Title(val title: String)

data class Timestamp(val unixTimestamp: Long)

data class Hash(val value: Long)

internal class Database {
    // for now let's assume that the DB will always return an Order for a
    // given Customer
    fun findOrder(customer: Customer): Order {
        return Order(
                Title(String.format("FP Workshop - %s", customer.name)),
                Timestamp(42L)
        )
    }
}

object Exercise6 {
    // Please do not change this function!
    private val getOrderForCustomer: (Customer, Database) -> Order =
            { customer, database ->
                database.findOrder(customer)
            }

    // Please do not change this function!
    private val getProductionDatabase =
            { Database() }

    // Please do not change this function!
    private val getOrderTitle: (Order) -> Title =
            { it.title }

    // Please do not change this function!
    private val getTitleHash: (Title) -> Hash =
            {
                Hash(it.title.length.toLong())
            }

    /*
    // this version is using flipping, currying, application of the first argument
    // and composition
    fun customerToHash(): (Customer) -> Hash =
            getOrderForCustomer
                    .flipped()
                    .curried()
                    .apF(getProductionDatabase)
                    .andThen(getOrderTitle)
                    .andThen(getTitleHash)
    */

    /*
    // this version is using currying, application of the second argument
    // and composition
    fun customerToHash(): (Customer) -> Hash =
            getOrderForCustomer
                    .curried()
                    .apS(getProductionDatabase)
                    .andThen(getOrderTitle)
                    .andThen(getTitleHash)
    */

    /*
    // this version is using flipping, application of the first argument
    // and composition
    fun customerToHash(): (Customer) -> Hash =
            getOrderForCustomer
                    .flipped()
                    .applyFirst(getProductionDatabase)
                    .andThen(getOrderTitle)
                    .andThen(getTitleHash)
    */

    /*
    // this is probably the easiest to grasp and read in Kotlin version
    // for anyone that is not used to functional programming
    fun customerToHash(): (Customer) -> Hash =
            getOrderForCustomer                          //(Customer, Database) -> Order
                    .applySecond(getProductionDatabase)  // Customer -> Order
                    .andThen(getOrderTitle)              // Customer -> Title
                    .andThen(getTitleHash)               // Customer -> Hash
    */

    // with infix notation without the dot and parenthesis noise
    // unfortunately Kotlin doesn't make it easier to split it into multiple lines
    fun customerToHash(): (Customer) -> Hash =
            getOrderForCustomer applySecond getProductionDatabase andThen getOrderTitle andThen getTitleHash
}
