@file:Suppress("PackageName", "unused")

package io.github.ajoz.workshop.fp.kotlin.part_3.solutions.exercise_3

import io.github.ajoz.workshop.fp.kotlin.part_3.solutions.exercise_3.FoodOrderEventType.OrderCreated

/*
  We can model this in two ways:

  1) Id is repeated for every type of an order event:

  type FoodOrderEvent =
                  | OrderCreated of Id * What * Where
                  | OrderWhatUpdated of Id * What
                  | OrderWhereUpdated of Id * Where
                  | OrderCanceled of Id
 */
sealed class FoodOrderEvent1 {
    data class OrderCreated(val id: Long, val what: String, val where: String) : FoodOrderEvent1()
    data class OrderWhatUpdated(val id: Long, val what: String) : FoodOrderEvent1()
    data class OrderWhereUpdated(val id: Long, val where: String) : FoodOrderEvent1()
    data class OrderCanceled(val id: Long) : FoodOrderEvent1()
}

/*
  or

  2) Id is a part of an event (we can guarantee that it exists always)
  type FoodOrderEventType =
                  | OrderCreated of What * Where
                  | OrderWhatUpdated of What
                  | OrderWhereUpdated of Where
                  | OrderCanceled

  type FoodOrderEvent = Id * FoodOrderEventType
 */
sealed class FoodOrderEventType {
    data class OrderCreated(val what: String, val where: String) : FoodOrderEventType()
    data class OrderWhatUpdated(val what: String) : FoodOrderEventType()
    data class OrderWhereUpdated(val where: String) : FoodOrderEventType()
    object OrderCanceled : FoodOrderEventType()
}

data class FoodOrderEvent2(val id: Long, val type: FoodOrderEventType)

fun main(args: Array<String>) {
    // First version:
    val foe1 = FoodOrderEvent1.OrderCreated(
            42L,
            "A very very very delicious food!",
            "Our office right around noon"
    )
    println(foe1)

    // Second version:
    val foe2 = FoodOrderEvent2(
            42L,
            OrderCreated(
                    "A super delicious and definitely not spicy food!",
                    "Our office but please be on time this time :>"
            )
    )
    println(foe2)
}
