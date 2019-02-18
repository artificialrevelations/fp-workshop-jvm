@file:Suppress("unused")

package io.github.ajoz.workshop.fp.part1.exercise7

/*
  -- Putting the knowledge to use --

  Functional programming is about building complexity from small parts. Let's
  use the knowledge we gained so far to solve the task below.

  You are working for a large company called EyeKeyA that is delivering ultra
  high quality furniture to the customers all over the world. Just as all other
  respected furniture companies, it has an IT department that is maintaining
  and improving company's software.

  You are on the team that maintains the Cutomers and Orders related modules.
  It's a very tough time as several teams including yours are working on a new
  exciting feature. You are tasked with creating a function that takes a customer
  information as an argument and returns a hash of the order that customer made.

  The module you and your team are working on already has a lot of useful
  functions:
  - to get the Order for a particular Customer
  - to get the Database from which Orders can be retrieved
  - to get the Metadata of the Order
  - to get the Hash from the given Metadata
 */

object Exercise7 {
    // Please do not change this function!
    private val getOrderForCustomer: (Customer, Database) -> Order =
            { customer, database ->
                database.findOrder(customer)
            }

    // Please do not change this function!
    private val getProductionDatabase = { Database() }

    // Please do not change this function!
    private val getOrderMetadata: (Order) -> Metadata = { it.metadata }

    // Please do not change this function!
    private val getTitleHash: (Metadata) -> Hash =
            {
                Hash(it.info.length.toLong())
            }

    fun getCustomerOrderHash(): (Customer) -> Hash =
            TODO("Exercise 7 getCustomerOrderHash is missing!")
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