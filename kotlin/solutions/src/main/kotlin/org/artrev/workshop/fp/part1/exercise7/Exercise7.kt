@file:Suppress("PackageName", "unused")

package org.artrev.workshop.fp.part1.exercise7

import io.github.ajoz.workshop.fp.tools.andThen
import io.github.ajoz.workshop.fp.tools.applySecond

object Exercise7 {
    private val getOrderForCustomer: (Customer, Database) -> Order =
            { customer, database ->
                database.findOrder(customer)
            }

    private val getProductionDatabase =
            { Database() }

    private val getOrderMetadata: (Order) -> Metadata =
            { it.metadata }

    private val getTitleHash: (Metadata) -> Hash =
            {
                Hash(it.info.length.toLong())
            }

    /*
    // this version is using flipping, currying, application of the first argument
    // and composition
    fun getCustomerOrderHash(): (Customer) -> Hash =
            getOrderForCustomer
                    .flipped()
                    .curried()
                    .apF(getProductionDatabase)
                    .andThen(getOrderMetadata)
                    .andThen(getTitleHash)
    */

    /*
    // this version is using currying, application of the second argument
    // and composition
    fun getCustomerOrderHash(): (Customer) -> Hash =
            getOrderForCustomer
                    .curried()
                    .apS(getProductionDatabase)
                    .andThen(getOrderMetadata)
                    .andThen(getTitleHash)
    */

    /*
    // this version is using flipping, application of the first argument
    // and composition
    fun getCustomerOrderHash(): (Customer) -> Hash =
            getOrderForCustomer
                    .flipped()
                    .applyFirst(getProductionDatabase)
                    .andThen(getOrderMetadata)
                    .andThen(getTitleHash)
    */


    // this is probably the easiest to grasp and read in Kotlin version
    // for anyone that is not used to functional programming
    fun getCustomerOrderHash(): (Customer) -> Hash =
            getOrderForCustomer                          //(Customer, Database) -> Order
                    .applySecond(getProductionDatabase)  // Customer -> Order
                    .andThen(getOrderMetadata)           // Customer -> Metadata
                    .andThen(getTitleHash)               // Customer -> Hash

}


// other fields like: surname, address, etc.
data class Customer(val name: String)

// other fields like: amount, currency, tax, etc.
data class Order(val metadata: Metadata, val date: Timestamp)

data class Metadata(val info: String)

data class Timestamp(val unixTimestamp: Long)

data class Hash(val value: Long)

internal class Database {
    // for now let's assume that the DB will always return an Order for a
    // given Customer
    fun findOrder(customer: Customer): Order {
        return Order(
                Metadata(String.format("FP Workshop - %s", customer.name)),
                Timestamp(42L)
        )
    }
}
