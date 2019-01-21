@file:Suppress("PackageName")

package io.github.ajoz.workshop.fp.kotlin.part_1.exercises.exercise_7

/*
  -- Putting the knowledge to use --

  Functional programming is about building complexity from small parts. Let's
  try to use the knowledge we gained so far to solve this simple example.

  Our apps domain is responsible for handling Customers and Orders. We
  already have a function implemented for retrieving Orders for a given Customer
  from a given Database. This function has a lot of domain logic and it is not
  feasible to refactor it now. We will have to use it while preparing our logic.

  One of the other teams we are working with prepared a code that expects a
  function from a Customer to hash (Long), but they would like us to provide
  the database for it.

  Our task is to write such function. We have all the necessary tools.

  Please pick and use methods/functions from previous exercises to finish this
  task.
 */

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

    fun customerToHash(): (Customer) -> Hash {
        TODO("Exercise 6 customerToHash is missing!")
    }
}